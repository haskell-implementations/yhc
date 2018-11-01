import IO
filename = "yhc_temp_test_file"
main = do writeFile filename (replicate 50 'a')
          file <- openFile filename ReadWriteMode
          hSeek file AbsoluteSeek 20
          hPutStr file "1234567890"
          hClose file
          s <- readFile filename
          hPutStrLn stdout s
