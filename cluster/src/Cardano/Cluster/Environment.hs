{-# LANGUAGE FlexibleContexts #-}

module Cardano.Cluster.Environment
    (
    -- * Environment
      Env
    , prepareEnvironment

    -- * State Directory
    , withStateDirectory

    -- * System Start
    , withSystemStart

    -- * Demo Configurations
    , demoTopology
    , demoTLSConfiguration
    ) where

import qualified Prelude
import           Universum hiding (keys, (%~), (.~), _2)

import           Control.Lens (Field2 (..), at, (%~), (.~), (?~))
import qualified Crypto.PubKey.RSA.Types as RSA
import           Data.Aeson (object, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import           Data.List ((!!))
import           Data.Map (Map, (!))
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromJust)
import qualified Data.Text as T
import           Data.Time (NominalDiffTime, addUTCTime, defaultTimeLocale,
                     formatTime, getCurrentTime)
import           Formatting (bprint, build, sformat, (%))
import qualified Formatting.Buildable
import           Network.TLS (PrivKey (PrivKeyRSA))
import           System.Directory (createDirectoryIfMissing)
import           System.FilePath (takeDirectory, (</>))
import           System.IO.Temp (withSystemTempDirectory)

import           Cardano.Cluster.Util (getsModify, indexedForM_, nextNtwrkAddr,
                     ntwrkAddrToBaseUrl, ntwrkAddrToNodeAddr,
                     ntwrkAddrToString, rotations, unsafeBoolFromString,
                     unsafeElemIndex, unsafeNetworkAddressFromString,
                     unsafeSeverityFromString, (|>))
import           Cardano.Wallet.Client.Http (WalletClient, mkHttpClient,
                     mkHttpsManagerSettings, newManager)
import           Cardano.X509.Configuration (CertConfiguration (..),
                     CertDescription (..), DirConfiguration (..),
                     ServerConfiguration (..), TLSConfiguration (..),
                     fromConfiguration, genCertificate)
import           Data.X509.Extra (CertificateChain (..), SignedCertificate,
                     genRSA256KeyPair, isClientCertificate, writeCertificate,
                     writeCredentials)
import           Network.Broadcast.OutboundQueue (MaxBucketSize (..))
import           Pos.Chain.Genesis (GeneratedSecrets (..), RichSecrets (..),
                     configGeneratedSecretsThrow, poorSecretToEncKey)
import           Pos.Core.NetworkAddress (NetworkAddress)
import           Pos.Infra.Network.DnsDomains (DnsDomains (..))
import           Pos.Infra.Network.Types (NodeName (..), NodeType (..))
import           Pos.Infra.Network.Yaml (AllStaticallyKnownPeers (..),
                     NodeMetadata (..), NodeRegion (..), NodeRoutes (..),
                     Topology (..))
import           Pos.Launcher.Configuration (ConfigurationOptions (..),
                     withConfigurations)
import           Pos.Util.Log.LoggerConfig (LoggerConfig)
import           Pos.Util.UserSecret (UserSecret, defaultUserSecret,
                     mkGenesisWalletUserSecret, usKeys, usPath, usPrimKey,
                     usVss, usWallet)
import qualified Pos.Util.UserSecret as UserSecret
import           Pos.Util.Wlog.Compatibility (usingNamedPureLogger)
import           Pos.Web.Types (TlsParams (..))


-- | Simple Type-Alias for readability. This mimics the actual ENV but in a pure
-- way. And bonus, we can use all sort of lenses and traversals on that!
type Env = Map String String


-- | Get Temporary Working / State Directory to work with.
--
-- use STATE_DIR as a directory if it's given,
-- otherwise, create a new system-level temp directory.
withStateDirectory
    :: Maybe FilePath     -- ^ A state directory, if Nothing, picks a random temp one
    -> (FilePath -> IO a) -- ^ Action to run with the state directory
    -> IO a
withStateDirectory mdir cb =
    case mdir of
        Nothing ->
            withSystemTempDirectory "cardano-sl-cluster" cb

        Just dir ->
            cb dir


-- | Get the local system time and hydrate the underlying environment.
--
-- Note that the system should be started "ahead" such that nodes can get
-- started once turned on.
withSystemStart
    :: NominalDiffTime -- ^ Offset ahead to the system should be started
    -> Env             -- ^ The underlying environment
    -> IO Env
withSystemStart n env = do
    systemStart <-
        formatTime defaultTimeLocale "%s" . addUTCTime n <$> getCurrentTime
    return
        (env & at "SYSTEM_START" ?~ systemStart)


-- | Setup the environment for the node. This is where we assign default values
-- to mandatory arguments and constructs the related configurations (topology,
-- logging, tls, ...).
--
-- It returns actions that can be ran at a higher level to create and get those
-- configurations as well as a modified ENV which has been hydrated with
-- everything needed by the node to start.
prepareEnvironment
    :: (NodeName, NodeType)     -- ^ Related node identifier
    -> [(NodeName, NodeType)]   -- ^ All nodes, including the related one
    -> FilePath                 -- ^ Node State / Working directory
    -> Env                      -- ^ ENVironment context with user-defined ENV vars
    -> ((IO GeneratedSecrets, IO Topology, IO LoggerConfig, IO (WalletClient IO)), Env)
prepareEnvironment node@(NodeName nodeIdT, nodeType) nodes stateDir = runState $ do
    modify withDefaultEnvironment
    genesis  <- getsModify prepareGenesis
    topology <- getsModify prepareTopology
    logger   <- getsModify prepareLogger
    tls      <- getsModify prepareTLS
    return (genesis, topology, logger, tls)
  where
    nodeId :: String
    nodeId = T.unpack nodeIdT

    withDefaultEnvironment :: Env -> Env
    withDefaultEnvironment =
        case nodeType of
            NodeCore  -> withDefaultCoreEnvironment
            NodeRelay -> withDefaultCoreEnvironment
            NodeEdge  -> withDefaultWalletEnvironment . withDefaultCoreEnvironment

    withDefaultCoreEnvironment :: Env -> Env
    withDefaultCoreEnvironment env = env
        & at "CONFIGURATION_FILE" %~ (|> "lib/configuration.yaml")
        & at "CONFIGURATION_KEY"  %~ (|> "default")
        & at "DB_PATH"            ?~ (stateDir </> "db" </> nodeId)
        & at "LISTEN"             %~ (|> "127.0.0.1:3000")
        & at "LOG_SEVERITY"       %~ (|> "Debug")
        & at "NODE_ID"            ?~ nodeId
        & at "REBUILD_DB"         %~ (|> "True")

    withDefaultWalletEnvironment :: Env -> Env
    withDefaultWalletEnvironment env = env
        & at "NO_CLIENT_AUTH"     %~ (|> "False")
        & at "WALLET_ADDRESS"     %~ (|> "127.0.0.1:8090")
        & at "WALLET_DB_PATH"     ?~ (stateDir </> "wallet-db" </> nodeId)
        & at "WALLET_DOC_ADDRESS" %~ (|> "127.0.0.1:8190")
        & at "WALLET_REBUILD_DB"  %~ (|> "True")

    -- | Generate secrets keys from a genesis configuration
    -- NOTE 'genesis-key' and 'keyfile' can't be overidden by ENV vars
    prepareGenesis :: Env -> (IO GeneratedSecrets, Env)
    prepareGenesis env =
        let
            keysPath :: FilePath
            keysPath =
                stateDir </> "generated-keys"

            cIndex :: Int
            cIndex =
                unsafeElemIndex node nodes


            configOpts :: ConfigurationOptions
            configOpts = ConfigurationOptions
                { cfoFilePath    = env ! "CONFIGURATION_FILE"
                , cfoKey         = toText (env ! "CONFIGURATION_KEY")
                , cfoSystemStart = Just 0
                , cfoSeed        = Nothing
                }

            getGeneratedSecrets :: ConfigurationOptions -> IO GeneratedSecrets
            getGeneratedSecrets opts = fst <$>
                ( usingNamedPureLogger "_"
                    $ withConfigurations Nothing Nothing False opts
                    $ \config _ _ _ -> configGeneratedSecretsThrow config
                )

            writeUserSecret :: UserSecret -> IO ()
            writeUserSecret secret = do
                createDirectoryIfMissing True (takeDirectory $ secret ^. usPath)
                UserSecret.writeRaw secret

            initGenesis :: IO GeneratedSecrets
            initGenesis = do
                gs <- getGeneratedSecrets configOpts

                let nRich = length $ gsRichSecrets gs
                let nCore = length $ filter isCoreNode nodes
                when (nRich < nCore) $ throwM (NotEnoughCoreNodes nRich nCore)

                -- NOTE
                -- _In theory_, we only need the corresponding rich key.
                indexedForM_ (gsDlgIssuersSecrets gs) $ \(sk, i) -> writeUserSecret $ defaultUserSecret
                    & usPrimKey ?~ sk
                    & usPath    .~ keysPath </> "dlg-issuers" </> (show i <> ".key")

                indexedForM_ (gsRichSecrets gs) $ \(rs, i) -> writeUserSecret $ defaultUserSecret
                    & usPrimKey ?~ rsPrimaryKey rs
                    & usVss     ?~ rsVssKeyPair rs
                    & usPath    .~ keysPath </> "rich" </> (show i <> ".key")

                indexedForM_ (gsPoorSecrets gs) $ \(ps, i) -> writeUserSecret $ defaultUserSecret
                    & usKeys    %~ (poorSecretToEncKey ps :)
                    & usWallet  ?~ mkGenesisWalletUserSecret (poorSecretToEncKey ps)
                    & usPath    .~ keysPath </> "poor" </> (show i <> ".key")

                return gs

            irrelevant :: IO GeneratedSecrets
            irrelevant = fail
                "Attempted to initialize genesis environment for a non-core node. \
                \This is seemingly irrelevant: only core nodes do validation and \
                \require such environment."
        in
            case nodeType of
                NodeCore ->
                    ( initGenesis
                    , env & at "KEYFILE" ?~ keysPath </> "rich" </> (show cIndex <> ".key")
                    )

                _ ->
                    (irrelevant, env)


    -- | Create the 'Topology' of the given node
    -- NOTE: The topology can't be overriden by ENV vars.
    prepareTopology :: Env -> (IO Topology, Env)
    prepareTopology env =
        let
            cIndex :: Int
            cIndex =
                unsafeElemIndex node nodes

            addr :: NetworkAddress
            addr =
                -- NOTE Safe when called after 'withDefaultEnvironment'
                unsafeNetworkAddressFromString (env ! "LISTEN")

            waddr :: NetworkAddress
            waddr =
                -- NOTE Safe when called after 'withDefaultEnvironment'
                unsafeNetworkAddressFromString (env ! "WALLET_ADDRESS")

            topologyPath :: FilePath
            topologyPath =
                stateDir </> "topology" </> T.unpack nodeIdT <> ".json"

            (nodeNames :: [NodeName], nodeTypes :: [NodeType]) =
                unzip nodes

            nodeAddrs :: [NetworkAddress]
            nodeAddrs = flip evalState (0, 0, 0) $ forM nodeTypes $ \typ -> do
                (c, r, w) <- get
                case typ of
                    NodeCore ->
                        put (c + 1, r, w) >> return (nextNtwrkAddr c addr)
                    NodeRelay ->
                        put (c, r + 1, w) >> return (nextNtwrkAddr (r + 100) addr)
                    NodeEdge ->
                        put (c, r, w + 1) >> return (nextNtwrkAddr w waddr)

            topology :: Topology
            topology =
                demoTopology nodeType (zip3 nodeNames nodeTypes nodeAddrs)

            initTopology :: IO Topology
            initTopology = do
                createDirectoryIfMissing True (takeDirectory topologyPath)
                BL.writeFile topologyPath (Aeson.encode topology)
                return topology
        in
            case nodeType of
                NodeEdge ->
                    ( initTopology
                    , env
                        & at "LISTEN"             .~ Nothing
                        & at "TOPOLOGY"           ?~ topologyPath
                        & at "WALLET_ADDRESS"     ?~ (ntwrkAddrToString $ nodeAddrs !! cIndex)
                        & at "WALLET_DOC_ADDRESS" ?~ (ntwrkAddrToString $ nextNtwrkAddr 100 (nodeAddrs !! cIndex))
                    )

                _ ->
                    ( initTopology
                    , env
                        & at "LISTEN"   ?~ (ntwrkAddrToString $ nodeAddrs !! cIndex)
                        & at "TOPOLOGY" ?~ topologyPath
                    )

    -- | Create a 'LoggerConfig' for the given node
    -- NOTE: The 'LoggerConfig' can't be overriden by ENV vars, however,
    -- the severity can be adjusted with an extra env var 'LOG_SEVERITY'
    prepareLogger :: Env -> (IO LoggerConfig, Env)
    prepareLogger env =
        let
            loggerConfigPath =
                stateDir </> "logs" </> nodeId <> ".json"

            logFilePath =
                stateDir </> "logs" </> nodeId <> ".log.pub"

            logSeverity =
                -- NOTE Safe when called after 'withDefaultEnvironment'
                unsafeSeverityFromString (env ! "LOG_SEVERITY")

            -- NOTE 1:
            -- Unfortunately, it appears that JSON instances of types from
            -- 'Pos.Util.Log.LoggerConfig' don't have the roundtrip property.
            -- Therefore, trying to parse a file generated from encoding a
            -- 'LoggerType' is hopeless.
            -- The representations don't match.
            loggerConfigJSON = object
                [ "rotation" .= object
                    [ "logLimit"  .= (104857600 :: Word64)
                    , "keepFiles" .= (1 :: Word)
                    ]
                , "loggerTree" .= object
                    [ "severity" .= logSeverity
                    , "files"    .= [ logFilePath ]
                    ]
                ]

            -- NOTE 'fromJust' is safe because we are making a valid JSON by hand.
            loggerConfig =
                fromJust $ Aeson.parseMaybe Aeson.parseJSON $ loggerConfigJSON

            initLoggerConfig = do
                createDirectoryIfMissing True (takeDirectory loggerConfigPath)
                BL.writeFile loggerConfigPath (Aeson.encode loggerConfigJSON)
                return loggerConfig
        in
            ( initLoggerConfig
            , env & at "LOG_CONFIG" .~ Just loggerConfigPath
            )

    -- | Create TLS Certificates configurations
    -- NOTE: The TLS configurations & certs can't be overriden by ENV vars.
    prepareTLS :: Env -> (IO (WalletClient IO), Env)
    prepareTLS env =
        let
            noClientAuth =
                -- NOTE Safe when called after 'withDefaultEnvironment'
                unsafeBoolFromString (env ! "NO_CLIENT_AUTH")

            wAddr =
                -- NOTE Safe when called after 'withDefaultEnvironment'
                unsafeNetworkAddressFromString (env ! "WALLET_ADDRESS")

            tlsBasePath =
                stateDir </> "tls" </> nodeId

            tlsParams = TlsParams
                { tpCertPath   = tlsBasePath </> "server.crt"
                , tpKeyPath    = tlsBasePath </> "server.key"
                , tpCaPath     = tlsBasePath </> "ca.crt"
                , tpClientAuth = not noClientAuth
                }

            (tlsConf, dirConf) =
                demoTLSConfiguration tlsBasePath

            initTLSEnvironment = do
                keys <- genRSA256KeyPair
                let (ca, cs) = fromConfiguration tlsConf dirConf genRSA256KeyPair keys
                (_, caCert) <- genCertificate ca
                -- NOTE Safe since we know there's at least one client cert
                clients <- forM cs $ \c -> do
                    createDirectoryIfMissing True (certOutDir c)
                    (key, cert) <- genCertificate c
                    writeCredentials (certOutDir c </> certFilename c) (key, cert)
                    writeCertificate (certOutDir c </> certFilename ca) caCert

                    if isClientCertificate cert then
                        Just <$> mkWalletClient wAddr caCert (cert, key)
                    else
                        return Nothing
                return $ Prelude.head $ catMaybes clients

            irrelevant = fail
                "Attempted to initialize TLS environment for a non-edge node. \
                \This is seemingly irrelevant: TLS is required for contacting \
                \the Wallet API."
        in
            case nodeType of
                NodeEdge ->
                    ( initTLSEnvironment
                    , env
                        & at "TLSCERT" ?~ tpCertPath tlsParams
                        & at "TLSKEY"  ?~ tpKeyPath tlsParams
                        & at "TLSCA"   ?~ tpCaPath tlsParams
                    )

                _ ->
                    (irrelevant, env)


-- | Demo TLS Configuration
demoTLSConfiguration
    :: FilePath -- ^ Directory to output TLS stuff
    -> (TLSConfiguration, DirConfiguration)
demoTLSConfiguration dir =
    ( TLSConfiguration
        { tlsCa = CertConfiguration
            { certOrganization = "IOHK - Demo"
            , certCommonName   = "Root Self-Signed CA"
            , certExpiryDays   = 365
            }
        , tlsServer = ServerConfiguration
            { serverAltNames      = "localhost" :| [ "127.0.0.1" ]
            , serverConfiguration = CertConfiguration
                { certOrganization = "IOHK - Demo"
                , certCommonName   = "Server Certificate"
                , certExpiryDays   = 365
                }
            }
        , tlsClients = pure CertConfiguration
            { certOrganization = "IOHK - Demo"
            , certCommonName   = "Client Certificate"
            , certExpiryDays   = 365
            }
        }
    , DirConfiguration
        { outDirServer  = dir
        , outDirClients = dir
        , outDirCA      = Just dir
        }
    )


-- | Create a default topology file structure for the given nodes associated
-- with their corresponding network addresses
demoTopology
    :: NodeType                               -- ^ Target node type (core, relay, edge ...)
    -> [(NodeName, NodeType, NetworkAddress)] -- ^ All fully qualified nodes
    -> Topology
demoTopology nodeType =
    case nodeType of
        NodeEdge ->
            TopologyBehindNAT 1 1 . mkRelays . filter isRelayNode
        _ ->
            TopologyStatic . mkStaticRoutes . filter (not . isEdgeNode)
  where
    mkRelays
        :: [(NodeName, NodeType, NetworkAddress)]
        -> DnsDomains a
    mkRelays =
        DnsDomains . pure . map (ntwrkAddrToNodeAddr . (^. _3))

    mkStaticRoutes
        :: [(NodeName, NodeType, NetworkAddress)]
        -> AllStaticallyKnownPeers
    mkStaticRoutes =
        AllStaticallyKnownPeers . Map.fromList . map mkStaticPeer . rotations

    mkStaticPeer
        :: ((NodeName, NodeType, NetworkAddress), [(NodeName, NodeType, NetworkAddress)])
        -> (NodeName, NodeMetadata)
    mkStaticPeer ((peerId, peerType, peerAddr), routes) =
        (peerId, mkStaticMetadata peerType peerAddr (mkRoutes routes))

    mkRoutes
        :: [(NodeName, NodeType, NetworkAddress)]
        -> NodeRoutes
    mkRoutes =
        NodeRoutes . map (pure . (^. _1))

    mkStaticMetadata
        :: NodeType
        -> NetworkAddress
        -> NodeRoutes
        -> NodeMetadata
    mkStaticMetadata nType (addr, port) routes = NodeMetadata
        { nmType       = nType
        , nmRegion     = NodeRegion "undefined"
        , nmRoutes     = routes
        , nmSubscribe  = DnsDomains []
        , nmValency    = 1
        , nmFallbacks  = 1
        , nmAddress    = ntwrkAddrToNodeAddr (addr, port)
        , nmKademlia   = False
        , nmPublicDNS  = False
        , nmMaxSubscrs = BucketSizeUnlimited
        }


--
-- (Internal) Helpers
--

data NotEnoughCoreNodes = NotEnoughCoreNodes
    { nKeys  :: Int
    , nNodes :: Int
    } deriving (Show)

instance Exception NotEnoughCoreNodes where
    displayException = toString . sformat build

instance Buildable NotEnoughCoreNodes where
    build e = bprint
        ("not enough rich keys ("%build%" keys) in provided configuration \
        \to cover for all Core nodes (" %build%" nodes).")
        (nKeys e)
        (nNodes e)


-- | Tell whether a tuple identifies a Relay node
isCoreNode
    :: Field2 t t NodeType NodeType
    => t
    -> Bool
isCoreNode =
    (== NodeCore) . (^. _2)


-- | Tell whether a tuple identifies a Relay node
isRelayNode
    :: Field2 t t NodeType NodeType
    => t
    -> Bool
isRelayNode =
    (== NodeRelay) . (^. _2)


-- | Tell whether a tuple identifies an Edge node
isEdgeNode
    :: Field2 t t NodeType NodeType
    => t
    -> Bool
isEdgeNode =
    (== NodeEdge) . (^. _2)


-- | Helper to create a WalletClient instance pointing at a given wallet node
mkWalletClient
    :: NetworkAddress
    -> SignedCertificate
    -> (SignedCertificate, RSA.PrivateKey)
    -> IO (WalletClient IO)
mkWalletClient wAddr@(host, port) ca (cert, key) = do
    let serverId = (B8.unpack host, B8.pack $ show port)
    let credentials = (CertificateChain [cert], PrivKeyRSA key)
    manager <- newManager $ mkHttpsManagerSettings serverId [ca] credentials
    return $ mkHttpClient (ntwrkAddrToBaseUrl wAddr) manager
