import Control.Applicative 
import Data.Char
import System.IO
import Data.List


data Variable = Variable {
    vname :: String,
    vtype :: String,
    value :: [[Int]]
} deriving Show

type Env = [Variable]
--e = [Variable{vname ="a", vtype = "Int", value = [[1]]}, Variable{vname ="b", vtype = "Int", value = [[2]]}]

newtype Parser a = P (Env -> String -> [(Env, a, String)])

parse :: Parser a -> Env -> String -> [(Env, a, String)]
parse (P p) env inp = p env inp

item :: Parser Char
item = P (\env inp -> case inp of
    [] -> []
    (x:xs) -> [(env,x,xs)])

-- parse (sat isLower) env "ciao"
sat :: (Char -> Bool) -> Parser Char
sat p = do {
    x <- item;
    if p x then return x else empty;
 }

space :: Parser ()
space = do {
    many(sat isSpace);
    return ();
}


token :: Parser a -> Parser a
token p = do {
    space;
    v <- p;
    space;
    return v;
}
 -- parse (symbol "ciao") e "ciao"
symbol :: String -> Parser String
symbol xs = token (string xs)

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

letter :: Parser Char
letter = sat isAlpha

alphanum :: Parser Char
alphanum = sat isAlphaNum

ident :: Parser String
ident = 
 do {
  x <- lower;
  xs <- many alphanum;
  return (x:xs);
 }

identifier :: Parser String
identifier = token ident

natural :: Parser Int
natural = token nat

integer :: Parser Int
integer = token int


--parse (some digit) e "12ab3"
digit :: Parser Char
digit = sat isDigit


nat :: Parser Int
nat = do {
    xs <- some digit;
    return (read xs);
}

char :: Char -> Parser Char
char x = sat (== x)

-- parse (token (string "ciao" )) e "ciaoiosono"
-- parse (string "ciao" ) e " ciaoiosono"

string :: String -> Parser String
string [] = return []
string (x:xs) = 
 do {
  char x;
  string xs;
  return (x:xs);
}


int :: Parser Int
int = 
 do {
  char '-';
  n <- nat;
  return (-n);
 }
 <|>
 nat;


-- Update the environment with a variable
-- If the variable is new (not declared before), it will be
-- added in the environment 
-- If the variable exstits, its value will be overwritten in.

updateEnv :: Variable -> Parser String
updateEnv var = P (\env input -> case input of
 xs -> [((modifyEnv env var),"",xs)])

modifyEnv :: Env -> Variable -> Env
modifyEnv [] var = [var]
modifyEnv (x:xs) newVar = if (vname x) == (vname newVar) then [newVar] ++ xs 
else [x] ++ modifyEnv xs newVar


-- Return the value of a variable given the vname
readVar :: String -> Parser Int
readVar vname = P (\env input -> case searchVar env vname of
	[[]] -> []
	[[value]] -> [(env,value, input)])

-- Search the value of a variable stored in the Env, given the vname
searchVar :: Env -> String -> [[Int]]
searchVar [] queryvname = []
searchVar (x:xs) queryvname = if (vname x) == queryvname 
	then [[((value x) !! 0) !! 0]] else searchVar xs queryvname

-- Defining functions to work on array

saveArray :: String -> [Int] -> Parser String
saveArray var val = updateEnv Variable{vname=var, vtype="Array", value= [val]}

saveStack :: String -> [Int] -> Parser String
saveStack var val = updateEnv Variable{vname=var, vtype="Stack", value= [val]}

saveQueue :: String -> [Int] -> Parser String
saveQueue var val = updateEnv Variable{vname=var, vtype="Queue", value= [val]}


saveMat :: String -> [[Int]] -> Parser String
saveMat var val = updateEnv Variable{vname=var, vtype="Matrix", value= val}


searchArrayVar :: Env -> String -> Int -> [[Int]]
searchArrayVar [] queryvname j = [[]]
searchArrayVar (x:xs) queryvname j = if ((vname x) == queryvname) 
	then [[((value x) !! 0) !! j]] else searchArrayVar xs queryvname j  


searchMatVar :: Env -> String -> Int -> Int -> [[Int]]
searchMatVar [] queryvname j k = []
searchMatVar (x:xs) queryvname j k = if ((vname x) == queryvname) 
    then [[((value x) !! j) !! k]] else searchMatVar xs queryvname j k   

readMatVar :: String -> Int -> Int -> Parser Int
readMatVar vname j k = P (\env input -> case searchMatVar env vname j k of
	[[]] -> []
	[[value]] -> [(env,value, input)])                             


updateMat ::[[Int]] -> Int -> Int -> Int ->[[Int]]
updateMat [[]] val row col = [[]]
updateMat (xs:ys) val 0 col = (updateArray xs val col):ys
updateMat (xs:ys) val row col = xs:(updateMat ys val (row-1) col)


updateArray ::[Int] -> Int -> Int ->[Int]
updateArray [] val col = []
updateArray (x:xs) val 0 = val:xs
updateArray (x:xs) val col = x:(updateArray xs val (col-1))
                                                 
readArrayVar :: String -> Int -> Parser Int
readArrayVar vname j = P (\env input -> case searchArrayVar env vname j of
	[[]] -> []
	[[value]] -> [(env,value, input)])

--read Values for matrix
searchVals :: Env -> String -> [[Int]]
searchVals [] queryvname = []
searchVals (x:xs) queryvname = if (vname x) == queryvname then value x else searchVals xs queryvname

readVals :: String -> Parser [[Int]]
readVals vname = P (\env input -> case searchVals env vname of
  [[]] -> []
  (xs:ys) -> [(env, xs:ys , input)])

----read Values for Array
readValsArray :: String -> Parser [Int]
readValsArray vname = P (\env input -> case searchValsArray env vname of
  [] -> []
  (xs:ys) -> [(env, xs:ys , input)])

searchValsArray :: Env -> String -> [Int]
searchValsArray [] queryvname = []
searchValsArray (x:xs) queryvname = if (vname x) == queryvname 
    then ((value x) !! 0) else searchValsArray xs queryvname

instance Functor Parser where
    -- fmap :: (a->b) -> Parser a -> Parser b
    fmap g p = P (\env input -> case parse p env input of 
        [] -> []
        [(env, v, out)] -> [(env, g v, out)])


instance Applicative Parser where 
    -- pure :: a -> Parser a
    pure v = P (\env input -> [(env, v, input)])
    -- <*> :: Parser(a -> b) -> Parser a -> Parser b
    pg <*> px = P(\env input -> case parse pg env input of 
        [] -> []
        [(env, g, out)] -> parse(fmap g px) env out)


instance Monad Parser where
    -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    p >>= f = P (\env input -> case parse p env input of
        [] -> []
        [(env, v, out)] -> parse(f v) env out)


instance Alternative Parser where
    -- empty :: Parser a
    empty = P (\env input -> [])
    -- (<|>) :: Parser a -> Parser a -> Parser a
    p <|> q = P (\env input -> case parse p env input of 
        [] -> parse q env input
        [(env, v, out)] -> [(env, v, out)])




-- ################## Arithmetic Expressions ########################

aexp :: Parser Int
aexp = (do 
    t <- aterm
    symbol "+"
    a <- aexp
    return (t+a))
    <|>
    (do 
    t <- aterm
    symbol "-"
    a <- aexp
    return (t-a))
    <|>
    aterm



aterm :: Parser Int
aterm = do {
    f <- afactor;
    symbol "*";
    t <- aterm;
    return (t * f);
    }
    <|>
    do {
    f <- afactor;
    symbol "/";
    t <- aterm;
    return (f `div` t);
    } <|> 
    do {
    f <- afactor;
    symbol "^";
    t <- afactor;
    return(power f t);
    } <|> 
    do {
    f <- afactor;
    symbol "%";
    t <- afactor;
    return(modul f t);
    } 
    <|>
    afactor


power :: Int -> Int -> Int
power _ 0 = 1
power 0 _ = 0
power a b = a * power a (b-1) 

modul :: Int -> Int -> Int
modul _ 0 = -1
modul 0 _ = 0
modul a b = if a < b then a else modul (a-b) b


afactor :: Parser Int
afactor = (do 
    symbol "("
    a <- aexp
    symbol ")"
    return a)
    <|>
    (do 
    i <- identifier
    readVar i)
    <|>
    integer


parseAexp :: Parser String
parseAexp = 
    do {
        t <- parseAterm;
        do {
            symbol "+";
            e <- parseAexp;
            return (t ++ "+" ++ e);
        }
        <|>
        do {
            symbol "-";
            e <- parseAexp;
            return (t ++ "-" ++ e);
        }
    } <|> do {
        t <- parseAterm;
        return t;
    }
    
parseAterm :: Parser String
parseAterm = do {
    f <- parseFactor;
    do {
        symbol "*";
        t <- parseAterm;
        return(f ++ "*" ++ t);
    } <|> do {
        symbol "/";
        t <- parseAterm;
        return(f ++ "/" ++ t);
    } <|> do {
        symbol "^";
        t <- parseAterm;
        return(f ++ "^" ++ t);
    }  <|> do {
        symbol "%";
        t <- parseAterm;
        return(f ++ "%" ++ t);
    }  <|>

    return f;
}

parseFactor :: Parser String
parseFactor = 
    do {
        symbol "(";
        e <- parseAexp;
        symbol ")";
        return ("(" ++ e ++ ")")
    } <|> do {
        symbol "-";
        f <- parseFactor;
        return("-" ++ f);
    } <|> do {
        i <- identifier;
        return i;
    } <|> do {
        i <- integer;
        return (show i);
    }


parseCompareTo :: Parser String
parseCompareTo = do {
    a1 <- parseAexp;
    symbol "==";
    a2 <- parseAexp;
    return (a1 ++ "==" ++ a2);
    } <|> do {
        a1 <- parseAexp;
        symbol "<=";
        a2 <- parseAexp;
        return (a1 ++ "<=" ++ a2);
    } <|> do {
        a1 <- parseAexp;
        symbol "<";
        a2 <- parseAexp;
        return (a1 ++ "<" ++ a2);
    } <|> do {
        a1 <- parseAexp;
        symbol ">=";
        a2 <- parseAexp;
        return (a1 ++ ">=" ++ a2);
    } <|> do {
        a1 <- parseAexp;
        symbol ">";
        a2 <- parseAexp;
        return (a1 ++ ">" ++ a2);
    } <|> do {
        a1 <- parseAexp;
        symbol "!=";
        a2 <- parseAexp;
        return (a1 ++ "!=" ++ a2);
    } 





-- ################## Boolean Expressions ########################

bexp :: Parser Bool
bexp = (do
    b0 <- bterm
    symbol "OR"
    b1 <- bexp
    return (b0 || b1))
    <|>
    bterm

bterm :: Parser Bool
bterm = (do 
    f0 <- bfactor
    symbol "AND"
    f1 <- bterm
    return (f0 && f1)
    <|>
    bfactor)

bfactor :: Parser Bool
bfactor = (do
        symbol "True"
        return True)
        <|>
        (do
        symbol "False"
        return False)
        <|>
        (do
        symbol "!"
        b <- bfactor
        return (not b))
        <|>
        (do 
            symbol "("
            b <- bexp
            symbol ")"
            return b)
        <|>
        bcomparison

bcomparison :: Parser Bool
bcomparison = (do 
    a0 <- aexp
    symbol "=="
    a1 <- aexp
    return (a0 == a1))
    <|>
    (do 
        a0 <- aexp
        symbol "<="
        a1 <- aexp
        return (a0 <= a1))
    <|>
    (do 
        a0 <- aexp
        symbol "<"
        a1 <- aexp
        return (a0 < a1))
    <|>
    (do 
        a0 <- aexp
        symbol ">="
        a1 <- aexp
        return (a0 >= a1))
    <|>
    (do 
        a0 <- aexp
        symbol ">"
        a1 <- aexp
        return (a0 > a1))
    <|>
    (do 
        a0 <- aexp
        symbol "!="
        a1 <- aexp
        return (a0 /= a1))

parseBexp :: Parser String
parseBexp = do {
    p1 <- parseBexp2;
    symbol "OR";
    p2 <- parseBexp;
    return (p1 ++ " OR " ++ p2);
    } <|> do {
        p <- parseBexp2;
        return p;
    }

parseBexp2 :: Parser String
parseBexp2 = 
    do {
        p1 <- parseBexp3;
        symbol "AND";
        p2 <- parseBexp2;
        return (p1 ++ " AND " ++ p2);
    }
    <|>
    do {
        c <- parseCompareTo;
        return c;
    }
    <|>
    do {
        symbol "True";
        return "True";
    }
    <|>
    do {
        symbol "False";
        return "False";
    }
    <|>
    do {
        i <- identifier;
        return i;
    }
    <|>
    do {
        symbol "!";
        p <- parseBexp3;
        return ("!" ++ p)
    }

parseBexp3 :: Parser String
parseBexp3 =
    do {
        symbol "(";
        p <- parseBexp;
        symbol ")";
        return ("(" ++ p ++ ")");
    }
    <|>
    do {
        c <- parseCompareTo;
        return c;
    }
    <|>
    do {
        symbol "True";
        return "True";
    }
    <|>
    do {
        symbol "False";
        return "False";
    }
    <|>
    do {
        i <- identifier;
        return i;
    }
    <|>
    do {
        symbol "!";
        p <- parseBexp3;
        return ("!" ++ p)
    }



fromBoolToInt :: Bool -> Int
fromBoolToInt True = 1
fromBoolToInt _ = 0



-- ################## Commands ########################


assignment :: Parser String
assignment = (do 
    --x:=10;
    x <- identifier
    symbol ":="
    v <- aexp
    symbol ";"
    updateEnv Variable{vname = x, vtype = "Integer", value = [[v]]})
    <|>
    (do
    --x:=True;
    x <- identifier
    symbol ":="
    v <- bexp
    symbol ";"
    updateEnv Variable{vname = x, vtype = "Boolean", value = [[(fromBoolToInt v)]]})
    <|>
    -- x:={1,2,3};
    (do 
	id <- identifier
	symbol ":="
	arr <- array
	symbol ";"
	saveArray id arr)
    <|>
	-- x:={1,2,3}; y:=x[1];
	(do 
	id <- identifier
	symbol ":="
	id2 <- identifier
	symbol "["
	index <- aexp
	symbol "]"
	symbol ";"
	val <- readArrayVar id2 index
	updateEnv Variable{vname = id, vtype = "Integer", value = [[val]]})
	
	<|>
    -- x := {1,2,3}++{4,5,6};
    (do 
	id <- identifier
	symbol ":="
	ar1 <- array
	symbol "++"
	ar2 <- array
	symbol ";"
	saveArray id (ar1 ++ ar2))
    <|>
    -- y:= {1,2,3}; z:={4,5,6}; x := y++z;
    (do 
	id <- identifier
	symbol ":="
	id1 <- identifier
	symbol "++"
	id2 <- identifier
	symbol ";"
	arr1 <- readValsArray id1
	arr2 <- readValsArray id2
	saveArray id (arr1++arr2))
    <|>
    -- createArray(v[5]);
    (do 
	symbol "createArray"
	symbol "("
	id <- identifier
	symbol "["
	col <- aexp
	symbol "]"
	symbol")"
	symbol ";"
	saveArray id (createArray [] col))
    <|>
    -- x:={{1,2,3},{1,2,3}};
    (do 
	id <- identifier
	symbol ":="
	mat <- matrix
	symbol ";"
	saveMat id mat)
    <|>

    -- x:={{1,2,3},{1,2,3}}; x[1][1]:= 3;
    (do 
	id <- identifier
	symbol "["
	row <- aexp
	symbol "]"
	symbol "["
	col <- aexp
	symbol "]"
	symbol ":="
	val <- aexp
	symbol ";"
	mat <- readVals id
	saveMat id (updateMat mat val row col))
    <|>
    -- x:={{1,2,3},{4,5}}; y:=x[1][1];
    (do 
	id <- identifier
	symbol ":="
	id2 <- identifier
	symbol "["
	row <- aexp
	symbol "]"
	symbol "["
	col <- aexp
	symbol "]"
	symbol ";"
	val <- readMatVar id2 row col;
	updateEnv Variable{vname = id, vtype = "Integer", value = [[val]]})
     <|>
    -- x:={1,2,3}; x[1]:= 3;
    (do 
	id <- identifier
	symbol "["
	col <- aexp
	symbol "]"
	symbol ":="
	val <- aexp
	symbol ";"
	arr <- readValsArray id
	saveArray id (updateArray arr val col))
    <|>
    -- x:={1,2,3}; y:= {1,2,3}; x[1]:=y[2];
        (do 
		id <- identifier
		symbol "["
		index <- aexp
		symbol "]"
		symbol ":="
		id2 <- identifier
		symbol "["
		index2 <- aexp
		symbol "]"
		symbol ";"
		arr <- readValsArray id
		val <- readArrayVar id2 index2 
		saveArray id (updateArray arr val index))
    <|>
    -- createMat(v[5][5]);
    (do
	symbol "createMat("
	id <- identifier
	symbol "["
	row <- aexp
	symbol "]"
	symbol "["
	col <- aexp
	symbol "])"
	symbol ";"
	saveMat id (createMat [[]] row col))
    <|>
    -- x:={1,2,3}; push(x,2);
        (do
            symbol "push("
            id <- identifier
            symbol ","
            val <- aexp 
            symbol ");"
            stck <- readValsArray id
            saveStack id ([val] ++ stck)
            )
    <|>
        -- x:={1,2,3}; pop(x);
        (do
            symbol "pop("
            id <- identifier
            symbol ");"
            stck <- readValsArray id
            saveStack id (tail stck)
            )
    <|>     
        -- x:={1,2,3}; enqueue(x,2);
        (do
            symbol "enqueue("
            id <- identifier
            symbol ","
            val <- aexp 
            symbol ");"
            q <- readValsArray id
            saveQueue id ([val] ++ q)
            )
    <|>
        -- x:={1,2,3}; dequeue(x);
        (do
            symbol "dequeue("
            id <- identifier
            symbol ");"
            q <- readValsArray id
            saveQueue id (init q)
            )    




skip :: Parser String
skip = do {
    symbol "skip";
    symbol ";";
    parseCommand;
}

--x:=0; repeat{x:=x+1;} until (x<10)
repeatUntil :: Parser String
repeatUntil = do 
    w <- parseRepeatUntil;
	repeatWhile w;
	symbol "repeat";
	symbol "{";
	program;
	symbol "}";
	symbol "until";
    symbol "(";
    b <- bexp;
    symbol ")";
	if (b) then do{
		repeatWhile w;
		repeatUntil;
	} else do{
		return "";
	}
    
--x:=6; if (x<5) {x:= x+1;} else {x:=x-1;}
ifThenElse :: Parser String
ifThenElse = (do
    symbol "if"
    symbol "("
    b <- bexp
    symbol ")"
    symbol "{"
    if (b) then
        (do
            program
            symbol "}"
            (do 
                symbol "else"
                symbol "{"
                parseProgram;
                symbol "}"
                return "")
            <|>
            (return ""))
    else
        (do
            parseProgram
            symbol "}"
            (do
                symbol "else"
                symbol "{"
                program
                symbol "}"
                return "")
            <|>
            return "")
        )


forLoop :: Parser String
forLoop = do
    f <- parseForLoop
    repeatWhile f
    program
    return ""

-- x:=0; while (x<10) {x:=x+1;}

while :: Parser String
while = do 
    w <- parseWhile
    repeatWhile w
    symbol "while"
    symbol "("
    b <- bexp
    symbol ")"
    symbol "{"
   
    if (b) then (
        do
            program
            symbol "}"
            repeatWhile w
            while)
    else (
        do
            parseProgram
            symbol "}"
            return "")

command :: Parser String
command = assignment
    <|>
    ifThenElse
    <|>
    while
    <|>
    forLoop
    <|>
    repeatUntil
    <|>
    (do
        symbol "skip"
        symbol ";")


program :: Parser String
program = (do 
    command
    program)
    <|>
    command


parseAssignment :: Parser String
parseAssignment = -- y := x[1]
              (do 
			  id <- identifier
			  symbol ":="
			  id2 <- identifier
			  symbol "["
			  index <- parseAexp
			  symbol "]"
			  symbol ";")-- added parentesi
              <|>
              -- x[1] := y[1];
              (do 
			  id <- identifier
			  symbol "["
			  index <- parseAexp
			  symbol "]"
			  symbol ":="
			  id2 <- identifier
			  symbol "["
			  index2 <- parseAexp
			  symbol "]"
			  symbol ";"
			  return $ id ++ "[" ++ index ++ "]:=" ++ id2 ++ "[" ++ index2 ++ "]" ++ ";")
              <|>
                  (do 
				  id <- identifier
				  symbol ":="
				  a <- parseAexp
				  symbol ";"
				  return $ id ++ ":=" ++ a ++ ";")
                <|>
                  (do 
				  id <- identifier
				  symbol ":="
				  a <- parseBexp
				  symbol ";"
				  return $ id ++ ":=" ++ a ++ ";")
              <|>
            -- x[1] := y
              (do 
			  id <- identifier
			  symbol "["
			  index <- parseAexp
			  symbol "]"
			  symbol ":="
			  val <- parseAexp
			  array <- parseArray
			  symbol ";"
			  return $ id ++ "[" ++ index ++ "]" ++ ":=" ++ val ++ ";" )
            <|>
              -- x:={1,2,3}
              (do 
			  id <- identifier
			  symbol ":="
			  arr <- parseArray
			  symbol ";"
			  return $ id ++ ":=" ++ arr ++ ";" )
            <|>
              -- x={{1,2,3},{1,2,3}}
              (do 
			  id <- identifier
			  symbol ":="
			  m <- parseMat
			  symbol ";"
			  return $ id ++ ":=" ++ m ++ ";" )
             <|>
              -- x = y++z
              (do 
			  id <- identifier
			  symbol ":="
			  ar1 <- parseArray
			  symbol "++"
			  ar2 <- parseArray
			  symbol ";"
			  return $ id ++ ":=" ++ ar1 ++ "++" ++ ar2 ++ ";")  
            
            <|>
            -- x={{1,2,3},{1,2,3}};
                (do 
				id <- identifier
				symbol ":="
				symbol "{"
				m <- parseMat
				symbol "}"
				symbol ";"
				return $ id ++ ":=" ++ "{" ++ m ++ "}"++";")
            <|>
                --y:={{1,2,3},{1,2,3}}; x:=y[1][1];
                (do 
				id <- identifier
				symbol ":="
				id2 <- identifier
				symbol "["
				row <- parseAexp
				symbol "]"
				symbol "["
				col <- parseAexp
				symbol "]"
				symbol ";"
				return $ id ++ ":=" ++ id2 ++"[" ++ row ++ "]" ++"[" ++ col ++ "]" ++";")
            <|>
    -- x={{1,2,3},{1,2,3}}; x[1][1]:= 3;
                (do 
				id <- identifier
				symbol "["
				row <- parseAexp
				symbol "]"
				symbol "["
				col <- parseAexp
				symbol "]"
				symbol ":="
				val <- parseAexp
				symbol ";"
				return $ id ++ "[" ++ row ++ "]" ++"[" ++ col ++ "]" ++ ":=" ++ val ++";")
             <|>
        -- x:={1,2,3}; push(x,2);
            (do
                symbol "push("
                id <- identifier
                symbol ","
                val <- parseAexp 
                symbol ");"
                return $ "push(" ++ id ++ "," ++ val ++ ");"
                )
                <|>
            -- x:={1,2,3}; pop(x);
            (do
                symbol "pop("
                id <- identifier
                symbol ");"
                return $ "pop(" ++ id ++ ");"
                )  
                <|>     
            -- x:={1,2,3}; enqueue(x,2);
            (do
                symbol "enqueue("
                id <- identifier
                symbol ","
                val <- parseAexp 
                symbol ");"
                return $ "enqueue(" ++ id ++ "," ++ val ++ ");"
                )
                <|>
            -- x:={1,2,3}; dequeue(x);
            (do
                symbol "dequeue("
                id <- identifier
                symbol ");"
                return $ "dequeue(" ++ id ++ ");"
                )       
                <|>
            -- createArray(v[5]);
            (do 
			symbol "createArray"
			symbol "("
			id <- identifier
			symbol "["
			col <- parseAexp
			symbol "]"
			symbol ")"
			symbol ";"
			return $ "createArray"++"(" ++ id ++ "[" ++ col ++ "]"++")"++";")
                <|>
    -- createMat(v[5][5]);
            (do 
			symbol "createMat("
			id <- identifier
			symbol "["
			row <- parseAexp
			symbol "]"
			symbol "["
			col <- parseAexp
			symbol "])"
			symbol ";"
			return $ "createMat(" ++ id ++ "[" ++ row ++ "]" ++ "[" ++ col ++ "]);")
                        


parseCommand :: Parser String
parseCommand = 
    do {
        a <- parseAssignment;
        return a;
    }
    <|>
    do {
        s <- parseSkip;
        return s;
    }
    <|>
    do {
        i <- parseIfThenElse;
        return i;
    }
    <|>
    do {
        w <- parseWhile;
        return w;
    }
    <|>
    do {
        f <- parseForLoop;
        return f;
    }
    


parseProgram :: Parser String
parseProgram = do {
    c <- parseCommand;
    p <- parseProgram;
    return (c ++ p);
} <|> do {
    c <- parseCommand;
    return c;
}




parseSkip :: Parser String
parseSkip = do {
    symbol "skip";
    symbol ";";
    c <- parseCommand;
    return ("skip;" ++ c)
}




parseRepeatUntil :: Parser String
parseRepeatUntil = do{
	symbol "repeat";
	symbol "{";
	p <- parseProgram;
	symbol "}";
	symbol "until";
	symbol "(";
	b <- consumeBexp;
	symbol ")";
	return("repeat {" ++ p ++ "} until " ++ "( " ++ b ++ " ) " );
}




parseIfThenElse :: Parser String
parseIfThenElse = do {
    symbol "if";
    symbol "(";
    b <- parseBexp;
    symbol ")";
    symbol "{";
    p1 <- parseProgram;
    symbol "}";
    do {
        symbol "else";
        symbol "{";
        p2 <- parseProgram;
        symbol "}";
        return ("if(" ++ b ++ "){" ++ p1 ++ "}else{" ++ p2 ++ "}")
    }
    <|>
    return ("if(" ++ b ++ "){" ++ p1 ++ "}");
}


parseWhile :: Parser String
parseWhile = do {
    symbol "while";
    symbol "(";
    b <- parseBexp;
    symbol ")";
    symbol "{";
    p <- parseProgram;
    symbol "}";
    return ("while(" ++ b ++ "){" ++ p ++ "}");
}

repeatWhile :: String -> Parser String
repeatWhile c = P(\env input -> [(env, "", c ++ input)])


consumeBexp :: Parser String
consumeBexp = do
    b <- parseBexp
    return b

-- for (i:=0; i<9; i:=i-1;)  {a:=a+1;}
parseForLoop :: Parser String
parseForLoop = do {
    symbol "for";
    symbol "(";
    a <- parseAssignment;
    b <- parseBexp;
    symbol ";";
    x <- identifier;
    symbol "++";
    symbol ")";
    symbol "{";
    p <- parseProgram;
    symbol "}";
    return (a ++ " while(" ++ b ++ ") {" ++ p ++ x ++ ":=" ++ x ++ "+1;}");
} <|> do {
    symbol "for";
    symbol "(";
    a <- parseAssignment;
    b <- parseBexp;
    symbol ";";
    x <- identifier;
    symbol "--";
    symbol ")";
    symbol "{";
    p <- parseProgram;
    symbol "}";
    return (a ++ " while(" ++ b ++ ") {" ++ p ++ x ++ ":=" ++ x ++ "-1;}");
} <|> do {
    symbol "for";
    symbol "(";
    a <- parseAssignment;
    b <- parseBexp;
    symbol ";";
    c <- parseAssignment;
    symbol ")";
    symbol "{";
    p <- parseProgram;
    symbol "}";
    return (a ++ " while(" ++ b ++ ") {" ++ p ++ c ++ "}");
}

-- Defining Vectors
-- x = {1,2,3,4}
-- x{1} = 2 -- access from index
-- concatenate 2 arrays
-- x = y -- save an array into another
-- y = x{i} + y -- use an element of an array as a factor


parseConcArray :: Parser String
parseConcArray = (do a <- parseArray
                     symbol "++"
                     b <- parseConcArray
                     return ( a ++ b))
                 <|>
                     parseArray 

parseArrayItems :: Parser String
parseArrayItems = (do a <- parseAexp
                      symbol ","
                      b <- parseArrayItems
                      return (a ++ "," ++ b))
                <|> parseAexp

parseArray :: Parser String
parseArray = (do symbol "{"
                 a <- parseArrayItems
                 symbol "}"
                 return ("{" ++ a ++ "}"))
              <|> identifier

parseMatItems :: Parser String
parseMatItems = do {do { a <- parseArray;
                      symbol ",";
                      b <- parseMatItems;
                      return (a ++ "," ++ b);}
                <|> do { parseArray;}}

parseMat :: Parser String
parseMat = do{do {symbol "{";
                 a <- parseMatItems;
                 symbol "}";
                 return ("{" ++ a ++ "}");}
              <|> do {identifier }}            
              
              
concArray :: Parser [Int]
concArray = (do a <- array
                symbol "++"
                b <- concArray
                symbol ";"
                return (a ++ b))
            <|> array

arrayItems :: Parser [Int]
arrayItems = (do a <- aexp
                 symbol ","
                 as <- arrayItems
                 return ([a] ++ as))
             <|>
             (do a <- aexp
                 return [a])  

matrixItems :: Parser [[Int]]
matrixItems = do {
    do {
    a <- array;
    symbol ",";
    as <- matrixItems;
    return ([a] ++ as);
       }   
    <|>
    do{
     a <- array;
    return ([a]);
    }
}

matrix :: Parser [[Int]]
matrix = (do symbol "{"
             a <- matrixItems
             symbol "}"
             return a)

array :: Parser [Int]
array =  (do symbol "{"
             a <- arrayItems
             symbol "}"
             return a)


createArray :: [Int] -> Int -> [Int]
createArray [] 0 = []
createArray [] col = [0] ++ (createArray [] (col-1))


createMat :: [[Int]] -> Int -> Int -> [[Int]]
createMat [[]] 0 col = []
createMat [[]] row col = (createArray [] col) : createMat [[]] (row -1) col

-- Extracts the GML code (it is "a" type) from tuple
getCode :: [(Env, a, String)] -> a
getCode [(_, x, _)]  =  x

booleanType = "Boolean"
integerType = "Integer"
floatType  = "Float"

getVarType :: Variable -> String
getVarType = vtype

getVarvname :: Variable -> String
getVarvname = vname

getVarValue :: Variable -> [[Int]]
getVarValue = value

--Extracts the Environment from the tuple and converts in a String form to print it
getMemory :: [(Env, a, String)] -> String
getMemory [] = " Invalid input\n"
getMemory [(x:xs, parsedString, "")] = case Main.getVarType x of
    "Boolean" -> case Main.getVarValue x of
        [[1]] -> " Boolean: " ++ (Main.getVarvname x) ++ " = True\n" ++ (getMemory [(xs,parsedString,"")])
        [[0]] -> " Boolean: " ++ (Main.getVarvname x) ++ " = False\n" ++ (getMemory [(xs,parsedString,"")])
    "Integer" -> " Integer: " ++ (Main.getVarvname x) ++ " = " ++ (show ((Main.getVarValue x !! 0) !! 0) ) ++ "\n" ++ (getMemory[(xs,parsedString,"")])
    "Array" -> " Array: " ++ (Main.getVarvname x) ++ " = " ++ (show (Main.getVarValue x !! 0) ) ++ "\n" ++ (getMemory[(xs,parsedString,"")])
    "Matrix" -> " Matrix: " ++ (Main.getVarvname x) ++ " = " ++ (show (Main.getVarValue x)) ++ "\n" ++ (getMemory[(xs,parsedString,"")])
    "Stack" -> " Stack: " ++ (Main.getVarvname x) ++ " = " ++ (show (Main.getVarValue x !! 0) ) ++ "\n" ++ (getMemory[(xs,parsedString,"")])
    "Queue" -> " Queue: " ++ (Main.getVarvname x) ++ " = " ++ (show (Main.getVarValue x !! 0) ) ++ "\n" ++ (getMemory[(xs,parsedString,"")])

    "" -> "Empty data type but we have following values: " ++ (Main.getVarvname x) ++ " = " ++ (show (Main.getVarValue x)) ++ "\n" ++ (getMemory[(xs,parsedString,"")])
    

getMemory [(env, parsedString, notParsedString)] = case notParsedString of
    "" -> ""
    otherwise -> " Error (unused input '" ++ notParsedString ++ "')\n" ++ getMemory [(env,parsedString, "")]


-- parse program [] "y:=3;if(y<3){z:=3;}"
-- getMemory(parse program [] "y:=3;if(y<3){z:=3;}")
parser :: String -> IO String
parser xs = 
    do
        putStr "IMPInt#>"
        hFlush stdout
        line <- getLine

        case line of
            "printmem" ->
                do
                    putStrLn  ""
                    putStrLn  " ***** Parsed code ***** "
                    if xs == [] then
                        putStrLn ""
                    else 
                        putStrLn (getCode (parse parseProgram [] xs))
                    putStrLn ""
                    putStrLn "***** Memory *****"
                    putStrLn (getMemory (parse program [] xs))
                    putStrLn ""
                    parser(xs)
            "syntax" ->
                do
                    putStrLn  "***** Interpreter - Syntax *****"
                    putStrLn  ""
                    putStrLn  " <digit> ::= 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 "
                    putStrLn  ""
                    putStrLn  " <nat> ::= <digit> <nat> | <digit> "
                    putStrLn  ""
                    putStrLn  " <integer> ::= [-] <nat> "
                    putStrLn  ""
                    putStrLn  " <identifier> ::= <lower> | <lower> <alphanum> "
                    putStrLn  ""
                    putStrLn  " <alphanum> ::= <upper> <alphanum> | <lower> <alphanum> | <nat> <alphanum> |"
                    putStrLn  "                <upper> | <lower> | <nat> "
                    putStrLn  ""
                    putStrLn  " <lower> ::= a-z "
                    putStrLn  ""
                    putStrLn  " <upper> ::= A-Z "
                    putStrLn  ""
                    putStrLn  " <aexp> ::= <aterm> '+' <aexp> | <aterm> '-' <aexp> | <aterm> "
                    putStrLn  ""
                    putStrLn  " <aterm> ::= <afactor> '*' <aterm> | <afactor> '/' <aterm> | <afactor> '^' <afactor> | <afactor> '%' <afactor> | <afactor>"
                    putStrLn  ""
                    putStrLn  " <afactor> ::= '('<aexp>')' | <integer> | <identifier> " 
                    putStrLn  ""
                    putStrLn  " <bexp> ::= <bterm> 'OR' <bexp> | <bterm> "
                    putStrLn  ""
                    putStrLn  " <bterm> ::= <bfactor> 'AND' <bterm> | <bfactor> "
                    putStrLn  ""
                    putStrLn  " <bfactor> ::= 'True' | 'False' | '!'<bfactor> | '('<bexp>')' | <bcomparison> "
                    putStrLn  ""
                    putStrLn  " <bcomparison> ::= <aexp> '==' <aexp> | <aexp> '<=' <aexp> | <aexp> '<' <aexp> | "
                    putStrLn  "                   <aexp> '>=' <aexp> | <aexp> '>' <aexp> | <aexp> '!=' <aexp> "
                    putStrLn  ""
                    putStrLn  " <program> ::= <command> | <command> <program> "
                    putStrLn  ""
                    putStrLn  " <command> ::= <assignment> | <ifThenElse> | <while> | <forLoop> | <repeatUntil> | skip';' "
                    putStrLn  ""
                    putStrLn  " <assignment> ::= <identifier> ':=' <aexp> ';' | <identifier> ':=' <bexp> ';' | "
                    putStrLn  " <identifier> ':=' <array> ';'|  <identifier> ':=' <array> '++' <array> ';' |  <identifier> ':=' <identifier> '++' <identifier> ';' | " 
                    putStrLn  " 'createArray('<identifier> '[' <aexp> ']);' |  <identifier> ':=' <matrix> ';' | <identifier> '[' <aexp> ']' '[' <aexp> ']' ':=' <aexp> ';' |"
                    putStrLn  " <identifier>':=' <identifier> '[' <aexp> ']' '[' <aexp> '];'| <identifier>':=' <identifier> '[' <aexp> '];'|"
                    putStrLn  "<identifier> '[' <aexp> ']:=' <identifier> '[' <aexp> '];'| 'createMat('<identifier> '[' <aexp> ']' '[' <aexp> ']);' |"
                    putStrLn  " <identifier> ':=' <stack> ';' | 'push('<identifier>','<aexp>');'| 'pop('<identifier>');' | <identifier> ':=' <queue> ';' |"
                    putStrLn  " 'enqueue(' <identifier>','<aexp>');'| 'dequeue('<identifier> ');'"
                    putStrLn  ""
                    putStrLn  " <array> ::= '{' <arrayItems> '}'| <identifier>"
                    putStrLn  ""
                    putStrLn  " <arrayItems> ::= <aexp> ',' <arrayItems> | <aexp>"
                    putStrLn  ""
                    putStrLn  " <matrix> ::= '{' <matrixItems> '}'| <identifier>"
                    putStrLn  ""
                    putStrLn  " <matrixItems> ::= <array> ',' <matrixItems> | <array>"
                    putStrLn  ""
                    putStrLn  " <stack> ::= '{' <arrayItems> '}'| <identifier>"
                    putStrLn  ""
                    putStrLn  " <queue> ::= '{' <arrayItems> '}'| <identifier>"
                    putStrLn  ""
                    putStrLn  " <ifThenElse> ::= 'if' '('<bexp>')' '{' <program> '}' |  'if' '('<bexp>')' '{' <program> '}' 'else' '{' <program> '}' "
                    putStrLn  ""
                    putStrLn  " <while> ::= 'while(' <bexp> ') {' <program> '}' "
                    putStrLn  ""
                    putStrLn  " <repeatUntil> ::= <parseRepeatUntil> 'repeat' '{' <program> '}' 'until' <bexp> <repeatUntil> | <parseRepeatUntil> 'repeat' '{' <program> '}' 'until' <bexp>"
                    putStrLn  ""
                    putStrLn  " <forLoop> ::= 'for(' <assignment> <bexp> ';' <identifier> '++) { ' <program> '}' |"
                    putStrLn  "               'for(' <assignment> <bexp> ';' <identifier> '--) { ' <program> '}' |"
                    putStrLn  "               'for(' <assignment> <bexp> ';' <assignment> ')   { ' <program> '}' " 
                    putStrLn  ""
                    parser (xs)
            "examples" ->
                do
                    putStrLn  "***** Program examples *****"
                    putStrLn  " Assignment :"
                    putStrLn  " bool := False; num := 7;"
                    putStrLn  ""
                    putStrLn  " ifThenElse :"
                    putStrLn  " x := 3; y := 4; if (x <= 4) { x := 76; } else { x := 88; }"
                    putStrLn  ""
                    putStrLn  " while :"
                    putStrLn  " n := 0; i := 0; while (i < 10) {n := n + 1; i := i + 1;}"
                    putStrLn  ""
                    putStrLn  " repeatUntil :"
                    putStrLn  " x:=0; repeat{x:=x+1;} until (x<10)"
                    putStrLn  ""
                    putStrLn  " for loop :"
                    putStrLn  " a:=0; for (i:=10; i>0; i--)  {a:=a+1;}"
                    putStrLn  ""
                    putStrLn  " Factorial of 3: "
                    putStrLn  " n := 3; i := 0; fact := 1; while (i < n) {fact := fact * (i+1); i := i+1;}"
                    putStrLn  ""
                    parser (xs)
            "help" ->
                do
                    putStrLn  "***** Help *****"
                    putStrLn  ""
                    putStrLn  " printmem        => Print the parsed code and the status of the memory"
                    putStrLn  ""
                    putStrLn  " syntax          => Show the BNF grammar for the Interpreter"
                    putStrLn  ""
                    putStrLn  " examples        => Examples of programs written in the grammar of the Interpreter"
                    putStrLn  ""
                    putStrLn  " help            => Print this help"
                    putStrLn  ""
                    putStrLn  " quit exit bye   => Stops Interpreter"
                    putStrLn  ""
                    parser (xs)
            "quit" ->
                do
                    return []
            "exit" ->
                do
                    return []
            "bye" ->
                do
                    return []
            otherwise -> 
                do
                    case parse parseProgram [] line of
                        [] -> 
                            do
                                putStrLn "Syntax error! Please read the syntax typing \"help\" "  
                                parser xs
                        otherwise -> 
                            do
                                parser(xs ++ line)
        return []


interpreter :: IO String
interpreter = do
    parser []

main = do   
    interpreter
