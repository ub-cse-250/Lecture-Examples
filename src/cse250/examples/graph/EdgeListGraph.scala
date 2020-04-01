package cse250.examples.graph

class EdgeListGraph[VType, EType] extends cse250.examples.types.mutable.GraphADT[VType, EType] {

  private class ListPosition[T](var _value: T, var _prev: ListPosition[T], var _next: ListPosition[T])

  private var _vertexListHead: ListPosition[Vertex] = null
  private var _vertexListTail: ListPosition[Vertex] = null
  private var _edgeListHead: ListPosition[Edge] = null
  private var _edgeListTail: ListPosition[Edge] = null

  case class EdgeListVertex(override protected val _elem: VType) extends Vertex {
    private[EdgeListGraph] val _position = new ListPosition[Vertex](this, null, null)
  }

  case class EdgeListEdge(override protected val _u: Vertex, override protected val _v: Vertex, override protected val _elem: EType) extends Edge {
    private[EdgeListGraph] val _position = new ListPosition[Edge](this, null, null)
  }

  override def insertVertex(label: VType): Vertex = {
    val v = EdgeListVertex(label)
    v._position._prev = _vertexListTail
    if (_vertexListHead == null) _vertexListHead = v._position
    else _vertexListTail._next = v._position
    _vertexListTail = v._position
    v
  }

  override def insertEdge(u: Vertex, v: Vertex, label: EType): Edge = {
    val e = EdgeListEdge(u, v, label)
    e._position._prev = _edgeListTail
    if (_edgeListHead == null) _edgeListHead = e._position
    else _edgeListTail._next = e._position
    _edgeListTail = e._position
    e
  }

  override def removeVertex(v: Vertex): Unit = {
    // Remove all edges v appears in from the graph.
    for (e <- this.incidentEdges(v)) this.removeEdge(e)
    // Remove v from vertex sequence.
    val pos = v.asInstanceOf[EdgeListVertex]._position
    if (_vertexListHead == pos) {
      _vertexListHead = _vertexListHead._next
      if (_vertexListHead == null) _vertexListTail = null
      else _vertexListHead._prev = null
    }
    else if (_vertexListTail == pos) {
      _vertexListTail = _vertexListTail._prev
      _vertexListTail._next = null
    }
    else {
      // Position interior to list. Relink around vertex.
      pos._prev._next = pos._next
      pos._next._prev = pos._prev
    }
  }

  override def removeEdge(e: Edge): Unit = {
    // Remove e from edge sequence.
    val pos = e.asInstanceOf[EdgeListVertex]._position
    if (_edgeListHead == pos) {
      _edgeListHead = _edgeListHead._next
      if (_edgeListHead == null) _edgeListTail = null
      else _edgeListHead._prev = null
    }
    else if (_edgeListTail == pos) {
      _edgeListTail = _edgeListTail._prev
      _edgeListTail._next = null
    }
    else {
      // Position interior to list. Relink around vertex.
      pos._prev._next = pos._next
      pos._next._prev = pos._prev
    }
  }

  override def vertices: Iterator[Vertex] = new Iterator[Vertex] {
    private var _currentVertexPosition = _vertexListHead

    override def hasNext: Boolean = _currentVertexPosition != null

    override def next(): Vertex = {
      val retval = _currentVertexPosition._value
      _currentVertexPosition = _currentVertexPosition._next
      retval
    }
  }

  override def edges: Iterator[Edge] = new Iterator[Edge] {
    private var _currentEdgePosition = _edgeListHead

    override def hasNext: Boolean = _currentEdgePosition != null

    override def next(): Edge = {
      val retval = _currentEdgePosition._value
      _currentEdgePosition = _currentEdgePosition._next
      retval
    }
  }

  override def incidentEdges(v: Vertex): Iterator[Edge] = new Iterator[Edge] {
    private var _currentEdgePosition = _edgeListHead
    while (_currentEdgePosition != null && !_currentEdgePosition._value.endVertices.contains(v))
      _currentEdgePosition = _currentEdgePosition._next

    override def hasNext: Boolean = _currentEdgePosition != null

    override def next(): Edge = {
      val retval = _currentEdgePosition._value
      _currentEdgePosition = _currentEdgePosition._next
      while (_currentEdgePosition != null && !_currentEdgePosition._value.endVertices.contains(v))
        _currentEdgePosition = _currentEdgePosition._next
      retval
    }
  }

  override protected def isAdjacentTo(u: Vertex, v: Vertex): Boolean = incidentEdges(u).indexWhere(e => e.endVertices.contains(v)) != -1
}
