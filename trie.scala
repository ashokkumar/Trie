case class Node(key: Char, var children: List[Node], isValidWord: Boolean = false)

case class Trie(roots: List[Node]) {
    def add(word: String) = {        
        def insert(node: Node, key: Char, isLast: Boolean): Node = {
            val child = Node(key, List.empty[Node], isLast)
            node.children = node.children ++ List(child)
            child
        }
        
        
        def add1(root: Node, ele: List[Char]): Node = {
            ele match {
                case Nil => root
                case h :: Nil => insert(root, h, true)
                case h :: tl => add1(insert(root, h, false), tl)
            }
        }

        val chars = word.toList
        val root = roots.find(_.key == chars.head)
        root match {
            case None => add1(Node(chars.head, List.empty[Node]), chars.tail)
            case Some(value) => add1(value, chars.tail)
        }        
    }
}