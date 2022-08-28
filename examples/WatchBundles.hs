import Pipes
import qualified Pipes.Prelude as P
import Pipes.VMCP.Marionette

surroundWith :: String -> Pipe String String IO ()
surroundWith delimiter =
  yield delimiter >> await >>= yield

main = runEffect $ do
  recvMarionetteMsgAsBundle "192.168.10.3" 39540
    >-> P.show
    >-> surroundWith "---"
    >-> P.stdoutLn
