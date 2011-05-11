import Data.List
import Network
import System.IO
import Text.Printf

server  = "irc.freenode.org"
port    = 6667
channel = "#stock"
nick    = "StockBot-GHC"

main = do
    h <- connectTo server (PortNumber (fromIntegral port))
    hSetBuffering h NoBuffering
    write h "NICK" nick
    write h "USER" (nick ++ " 0 * :wat wat")
    write h "JOIN" channel
    listen h
 
write :: Handle -> String -> String -> IO ()
write h s t = do
    hPrintf h "%s %s\r\n" s t
    printf    "> %s %s\n" s t

privmsg :: Handle -> String -> IO ()
privmsg h s = do
    write h "PRIVMSG" (channel ++ " :" ++ s)
 
listen :: Handle -> IO ()
listen h = forever $ do
    t <- hGetLine h
    let s = init t
    if ping s then pong s else eval h (clean s)
    putStrLn s
  where
    forever a = a >> forever a
    clean     = drop 1 . dropWhile(/= ':') . drop 1
    ping x    = "PING :" `isPrefixOf` x
    pong x    = write h "PONG" (':' : drop 6 x)

eval :: Handle -> String -> IO ()
eval h ".wat" = privmsg h "wat"
eval _ _      = return ()
