
data Trie a = TrieNodo (Maybe a) [(Char, Trie a)] deriving Eq

instance Show a => Show (Trie a) where
    show = showTrie ""
      where 
        showTrie :: Show a => String -> Trie a -> String
        showTrie indent (TrieNodo maybeValue children) =
            let valueLine = case maybeValue of
                                Nothing -> indent ++ "<vacío>\n"
                                Just v  -> indent ++ "Valor: " ++ show v ++ "\n"
                childrenLines = concatMap (\(c, t) -> showTrie (indent ++ "  " ++ [c] ++ ": ") t) children
            in valueLine ++ childrenLines


foldTrie:: (Maybe a->[(Char, b)]-> b) -> Trie a -> b 
foldTrie f (TrieNodo raiz hijos) =  f raiz (map (\(char, b) -> (char, foldTrie f b)) hijos)


triePrueba::Trie Bool
triePrueba = TrieNodo Nothing [('a', TrieNodo (Just True) [('j', TrieNodo Nothing [('m', TrieNodo (Just True) [])])]), ('c', TrieNodo Nothing [])]


--Definir la funcion caminos que, dado un Trie, devuelva las cadenas que denotan un camino
--entre la raız y cada uno de los nodos. No se deben incluir caminos repetidos.
--E.g., dado el siguiente Trie:
--t = TrieNodo Nothing
--[ (’a’, TrieNodo (Just True) []),
--(’b’, TrieNodo Nothing
--[(’a’, TrieNodo (Just True)
--[(’d’, TrieNodo Nothing [])])
--]),
--(’c’, TrieNodo (Just True) [])
--]
--caminos t ⇝ ["", "a", "b", "ba", "bad", "c"]



caminos :: Trie a -> [String]
caminos = foldTrie (\_ hijos recr -> (map (fst hijos))++ recr) 
