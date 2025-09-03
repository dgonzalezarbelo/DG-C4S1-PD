-- Daniel González Arbelo
import System.IO

-- Expresiones aritméticas
data ExpA = I Integer | V String | ExpA :+ ExpA | ExpA :- ExpA | ExpA :* ExpA
    deriving (Show, Read)

-- Expresiones booleanas
data ExpB = BoolLit Bool | ExpA :== ExpA | ExpA :< ExpA | ExpA :<= ExpA | ExpA :> ExpA | ExpA :>= ExpA| ExpB :&& ExpB | ExpB :|| ExpB | Not ExpB
    deriving (Show, Read)

-- Instrucciones
data Instr = String := ExpA | Cond ExpB [Instr] [Instr] | While ExpB [Instr]
    deriving (Show, Read)

-- Estado
data Estado = Estado [(String, Integer)]

-- Programa
type Programa = [Instr]

-- Definición para que se pueda mostrar un estado por pantalla
instance Show Estado where
    show (Estado vars) = "{" ++ showVars vars ++ "}"
      where
        showVars [] = ""
        showVars [(k, v)] = k ++ "=" ++ show v
        showVars ((k, v):rest) = k ++ "=" ++ show v ++ ", " ++ showVars rest

-- Consulta el valor de una variable en el estado
lookupVar :: String -> Estado -> Integer
lookupVar var (Estado vars) = case lookup var vars of
    Just val -> val
    Nothing  -> error $ "Variable no encontrada: " ++ var

-- Modifica el valor de una variable en el estado
updateVar :: String -> Integer -> Estado -> Estado
updateVar var val (Estado vars) = Estado $ (var, val) : filter (\(v, _) -> v /= var) vars

-- Tipo definido para representar expresiones aritméticas
evalExpA :: ExpA -> Estado -> Integer
evalExpA (I n) _ = n
evalExpA (V x) estado = lookupVar x estado
evalExpA (e1 :+ e2) estado = evalExpA e1 estado + evalExpA e2 estado
evalExpA (e1 :- e2) estado = evalExpA e1 estado - evalExpA e2 estado
evalExpA (e1 :* e2) estado = evalExpA e1 estado * evalExpA e2 estado

-- Tipo definido para representar expresiones booleanas
evalExpB :: ExpB -> Estado -> Bool
evalExpB (BoolLit b) _ = b
evalExpB (e1 :== e2) estado = evalExpA e1 estado == evalExpA e2 estado
evalExpB (e1 :< e2) estado = evalExpA e1 estado < evalExpA e2 estado
evalExpB (e1 :<= e2) estado = evalExpA e1 estado <= evalExpA e2 estado
evalExpB (e1 :> e2) estado = evalExpA e1 estado > evalExpA e2 estado
evalExpB (e1 :>= e2) estado = evalExpA e1 estado >= evalExpA e2 estado
evalExpB (e1 :&& e2) estado = evalExpB e1 estado && evalExpB e2 estado
evalExpB (e1 :|| e2) estado = evalExpB e1 estado || evalExpB e2 estado
evalExpB (Not e) estado = not (evalExpB e estado)

-- Función encargada de ejecutar una instrucción
ejecutaInstr :: Instr -> Estado -> Estado
ejecutaInstr (x := e) estado = updateVar x (evalExpA e estado) estado -- Instrucción de asignación
ejecutaInstr (Cond b p1 p2) estado = -- Instrucción condicional
    if evalExpB b estado then
        ejecutaPrograma p1 estado
    else
        ejecutaPrograma p2 estado
ejecutaInstr (While b p) estado = -- Instrucción de bucle
    if evalExpB b estado then
        ejecutaInstr (While b p) (ejecutaPrograma p estado)
    else
        estado

-- Función encargada de ejecutar una instrucción en el modo por pasos y devolver las que se tienen que añadir por delante a la
-- lista de instrucciones pendientes de ejecutar en ejecutaPorPasos
ejecutaInstrPorPasos :: Instr -> Estado -> (Programa, Estado)
ejecutaInstrPorPasos (x := e) estado = ([], updateVar x (evalExpA e estado) estado) -- Instrucción de asignación
ejecutaInstrPorPasos (Cond b p1 p2) estado = -- Instrucción condicional
    if evalExpB b estado then
        (p1, estado)
    else
        (p2, estado)
ejecutaInstrPorPasos (While b p) estado = -- Instrucción de bucle
    if evalExpB b estado then
        (p ++ [While b p], estado) -- Se añade el programa a ejecutar y la siguiente evaluación de la condición del While
    else
        ([], estado)

-- Ejecuta cada una de las instrucciones del programa
ejecutaPrograma :: Programa -> Estado -> Estado
ejecutaPrograma [] estado = estado
ejecutaPrograma (instr:instrs) estado = ejecutaPrograma instrs (ejecutaInstr instr estado)

-- Muestra por pantalla el valor de la variable "result" proveniente de ejecutar ejecutaPrograma
ejecuta :: Programa -> Estado -> Integer
ejecuta programa estado = lookupVar "result" (ejecutaPrograma programa estado)

-- Ejecuta paso por paso hasta que termine el programa o se teclee "q"
ejecutaPorPasos :: Programa -> Estado -> IO ()
ejecutaPorPasos programa estado = do
    putStrLn "Presiona Enter para ejecutar el siguiente paso (q + Enter para salir)"
    userInput <- getLine
    case userInput of
        "q" -> putStrLn "Ejecución aboratada"
        _   -> do
                if not (null programa) then
                    do
                        putStr "Instrucción ejecutada: "
                        print (head programa)
                        let (nuevasInstr, nuevoEstado) = ejecutaInstrPorPasos (head programa) estado
                        putStr "Nuevo estado: "
                        print nuevoEstado
                        ejecutaPorPasos (nuevasInstr ++ tail programa) nuevoEstado
                else
                    do
                        putStrLn "Programa terminado"

-- Función que lee la entrada para saber si el usuario quiere ejecutar la función por pasos
getUserInput :: IO Bool
getUserInput = do
    putStr "Pulsa Y + Enter si quieres ejecutar el programa paso por paso (otra tecla en caso contrario): "
    por_pasos <- getLine
    return (por_pasos == "Y")

-- La función factorial
factorial :: Programa
factorial = ["Y" := V "X",
                     "R" := I 1,
                     While (I 0 :< V "Y")
                        ["R" := (V "R" :* V "Y"),
                         "Y" := (V "Y" :- I 1)
                        ],
                    "result" := V "R"
                    ]

-- Estado de prueba
s0 :: Estado
s0 = Estado [("X", 5)]

main :: IO ()
main = do
    por_pasos <- getUserInput -- Preguntamos al usuario si quiere ejecutar por pasos
    if por_pasos then
        do
            putStrLn "Ejecutando el programa paso a paso:"
            ejecutaPorPasos factorial s0
    else
        do
            print (ejecuta factorial s0) -- Mostramos el estado final de todas las variables
