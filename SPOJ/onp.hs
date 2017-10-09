eachLine f = unlines . map f . lines

main :: IO ()
main = do
  input <- getLine
  let times = read input :: Int
  interact (eachLine $ solver "" [])

-- Op stack, symbol stack, exprression -> ans
solver :: String -> [String] -> String -> String

solver (o:op) (x:(y:symbol)) [] =
  solver op ((y ++ x ++ [o]) : symbol) []

solver "" (x:_) [] = x

solver op symbol ('(':expr) =
  solver ('(':op) symbol expr

solver op symbol (')':expr) =
  let
    (o:(_:op')) = op -- discard left parenthesis
    (x:(y:sym')) = symbol
    newSym = (y ++ x ++ [o]) : sym'
  in
    solver op' newSym expr

solver "" symbol ('-':expr) =
  solver "-" symbol expr

solver "" symbol ('*':expr) =
  solver "*" symbol expr

solver "" symbol ('/':expr) =
  solver "/" symbol expr

solver "" symbol ('^':expr) =
  solver "^" symbol expr

solver "" symbol ('+':expr) =
  solver "+" symbol expr

solver op symbol ('+':expr) =
  let
    (o:op') = op
    (x:(y:sym')) = symbol
    newSym = (y ++ x ++ [o]) : sym'
  in
    if o == '-' || o == '*' || o == '/' || o == '^' || o == '+'
      then solver op' newSym ('+':expr)
      else solver ('+':op) symbol expr

solver op symbol ('-':expr) =
  let
    (o:op') = op
    (x:(y:sym')) = symbol
    newSym = (y ++ x ++ [o]) : sym'
  in
    if o == '-' || o == '*' || o == '/' || o == '^'
      then solver op' newSym ('-':expr)
      else solver ('-':op) symbol expr

solver op symbol ('*':expr) =
  let
    (o:op') = op
    (x:(y:sym')) = symbol
    newSym = (y ++ x ++ [o]) : sym'
  in
    if o == '*' || o == '/' || o == '^'
      then solver op' newSym ('*':expr)
      else solver ('*':op) symbol expr

solver op symbol ('/':expr) =
  let
    (o:op') = op
    (x:(y:sym')) = symbol
    newSym = (y ++ x ++ [o]) : sym'
  in
    if o == '/' || o == '^'
      then solver op' newSym ('*':expr)
      else solver ('/':op) symbol expr

solver op symbol ('^':expr) =
  let
    (o:op') = op
    (x:(y:sym')) = symbol
    newSym = (y ++ x ++ [o]) : sym'
  in
    if o == '^'
      then solver op' newSym ('*':expr)
      else solver ('^':op) symbol expr

solver op symbol (alpha : expr) =
  solver op ([alpha] : symbol) expr
