{-# LANGUAGE TemplateHaskell #-}

module Main where
import Database.MongoDB             ( Field ( (:=) )
                                    , Value ( Bool
                                            , String
                                            )
                                    )
import Database.MongoDB.Connection  (host, connect)
import Database.MongoDB.Query       (access, master, insertMany, Action)
import Data.Data                    (Typeable)
import Data.Bson.Mapping
import Data.Text                    (pack, Text)

data Item = Item { content :: Text
                 , checked :: Bool
                 } deriving (Eq, Show, Typeable)

instance Bson Item where
  toBson (Item cont check) = [contLabel  := String cont
                             ,checkLabel := Bool check]
    where contLabel  = pack "content"
          checkLabel = pack "checked"

run :: Action IO ()
run = do
    insertItem $ Item (pack "Hey") False
    return ()

main :: IO ()
main = do
    -- connect: takes Socket & SocketAddress, returns  IO ()
    -- host:    takes a HostName (string), returns a IO Pipe
    pipe  <- connect $ host "127.0.0.1"
    -- access:  takes Pipe, AccessMode, Database & Action, returns MonadIO m with a
    -- master:  of type AccessMode (Same as ReadStaleOk)
    e     <- access pipe master (pack "todo") run
    let item = Item (pack "Hi") True
    print "Hi"

-- insertItem :: (Bson a) => (Item a) -> Action IO [Value]
insertItem i = do
    -- Takes Collection (Text), list of Document (Doc = list of Field)
    insertMany col [toBson i]
  where col = pack "first"
