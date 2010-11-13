import Controller
import Network.Wai.Handler.SimpleServer (run)

main :: IO ()
main = putStrLn "Loaded" >> withGraphmind (run 3000)
