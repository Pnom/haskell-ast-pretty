main = forM_ cmdReports $ \x -> do
               putStrLn $ "Writing report to " ++ x ++ " ..."
               writeReport x ideas