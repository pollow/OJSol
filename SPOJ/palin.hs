import qualified Data.ByteString.Char8 as BS

main :: IO ()
main = do
  input <- BS.getContents
  mapM_ (BS.putStrLn . solver . inc) (tail $ BS.lines input)

solver :: BS.ByteString -> BS.ByteString
solver ns = if (BS.reverse hs) >= ls
            then make hs
            else make $ inc hs
  where
    (hs, ls) = if even $ BS.length ns
               then (BS.take half ns, BS.drop half ns)
               else (BS.take (half + 1) ns, BS.drop half ns)
    make s = BS.append s (BS.drop res $ BS.reverse s)
    (half, res) = BS.length ns `divMod` 2

inc :: BS.ByteString -> BS.ByteString
inc s = case BS.readInteger s of
  Just (num, _) -> BS.pack . show $ num + 1
  Nothing -> BS.pack ""
