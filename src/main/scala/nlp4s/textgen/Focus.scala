package nlp4s.textgen

case class Focus[E](
  original: E, 
  stack: List[E],
  focusList: List[E],
  visited: Set[E],
) {
  def setPFL(fl: List[E]): Focus[E] = 
    copy(focusList = fl.filter { e => ! visited.contains(e) })

  def next(): List[Focus[E]] = {
    focusList.map(push(_)) ++ 
    List(this) ++ 
    (if(stack.nonEmpty) { List(pop()) } else { List.empty })
  }

  def push(e: E): Focus[E] =
    copy(stack = e::stack, visited = visited + e)

  def pop(): Focus[E] =
    if(stack.isEmpty) { this } else { copy(stack = stack.tail) }

  def immediateFocus = 
    stack.headOption.getOrElse(original)
}

object Focus {
  def init[E](originalFocus: E): Focus[E] = 
    Focus(originalFocus, List.empty, List.empty, Set.empty)
}
