module System.Remote.Monitoring.Riemann where
import Control.Concurrent
import Network.Socket
import System.Metrics (Store)

data RiemannConfig = RiemannConfig

connectRiemann :: RiemannConfig -> IO Socket
connectRiemann = undefined

sendRiemann :: Socket -> Store -> IO ()
sendRiemann = undefined

forkRiemann :: RiemannConfig -> Store -> IO ThreadId
forkRiemann = undefined
