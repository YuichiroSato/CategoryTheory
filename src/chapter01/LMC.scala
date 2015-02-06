/**
 * Created by Yuichiro on 2015/02/07.
 */
object LMC {

  case class Obj(ls: List[Int]) {

    def show(): Unit = print(this.toString)

    override def toString: String = {
      var str = "["
      if (0 < ls.size) str += ls.head.toString
      ls.drop(1) foreach { e =>
        str += " " + e.toString
      }
      str += "]"
      str
    }
  }

  object Obj {

    def apply(xs: Int*): Obj = Obj((xs.toList filter (_ > -1)).distinct.sorted)
  }

  case class Arrow(dom: Obj, cod: Obj, map: Map[Int, Int]) {

    def *(a: Arrow): Option[Arrow] = Arrow.compose(this, a)

    def **(als: List[Arrow]): List[Arrow] = als map (a => this * a) collect { case Some(a) => a}

    def show(): Unit = print(this.toString)

    override def toString: String = {
      def intToStr(i: Int): String = if (i == Arrow.emptyList) "()" else i.toString
      def mapToStr(map: Map[Int, Int]): String = {
        var str = "Map("
        if (0 < map.size) str += intToStr(map.head._1) + "->" + intToStr(map.head._2)
        map.drop(1) foreach { t =>
          str += ", " + intToStr(t._1) + "->" + intToStr(t._2)
        }
        str += ")"
        str
      }
      "dom " + dom.toString + " cod " + cod.toString + " " + mapToStr(map)
    }
  }

  object Arrow {

    val emptyList = -1

    def apply(dom: Obj, cod: Obj): List[Arrow] = make(dom, cod)

    def make(dom: Obj, cod: Obj): List[Arrow] = {
      if (dom.ls.isEmpty && cod.ls.isEmpty)
        List(Arrow(dom, cod, Map(emptyList -> emptyList)))
      else if (dom.ls.isEmpty)
        cod.ls map (a => Arrow(dom, cod, Map(emptyList -> a)))
      else if (dom.ls.size < cod.ls.size)
        makeMaps(dom, cod) map (m => Arrow(dom, cod, m))
      else if (dom.ls == cod.ls)
        List(Arrow(dom, cod, (dom.ls zip dom.ls).toMap))
      else
        List.empty[Arrow]
    }

    private def makeMaps(dom: Obj, cod: Obj): List[Map[Int, Int]] = makeImages(cod.ls, dom.ls.size) map (ls => dom.ls zip ls) map (_.toMap)

    private def makeImages(ls: List[Int], n: Int): List[List[Int]] = {
      var images = ls map (a => List(a))
      for (_ <- 1 to n - 1) {
        images = images flatMap (im => ls map (a => a :: im))
      }
      images
    }

    def compose(a1: Arrow, a2: Arrow): Option[Arrow] = {
      if (a1.cod == a2.dom)
        Some(Arrow(a1.dom, a2.cod, a1.map map (t => (t._1, a2.map.get(t._2).get)) toMap))
      else
        None
    }
  }

  case class Category(objects: List[Obj], category: Map[(Obj, Obj), List[Arrow]]) {

    def getObjects: List[Obj] = objects

    def getArrows(obj: Obj): List[Arrow] = {
      val keys = (category.keys filter (t => t._1 == obj)).toList
      keys map (key => category.get(key)) collect { case Some(a) => a} flatten
    }

    def getArrows(obj1: Obj, obj2: Obj): List[Arrow] = category.getOrElse((obj1, obj2), List.empty[Arrow])

    def getCategory: Map[(Obj, Obj), List[Arrow]] = category

    def showObjects(): Unit = {
      println("objects")
      objects foreach { obj =>
        print(obj.toString + " ")
      }
      println()
    }

    def showArrows(): Unit = {
      println("arrows")
      objects foreach { obj =>
        getArrows(obj) foreach { arrow =>
          arrow.show()
          println()
        }
      }
    }

    def show(): Unit = {
      showObjects()
      showArrows()
    }
  }

  object Category {

    def apply(n: Int): Category = {
      val objs = makeAllObjects(n)
      val ids = (objs map (obj => ((obj, obj), Arrow(obj, obj)))).toMap
      val arrows = (for (obj1 <- objs; obj2 <- objs; if obj1.ls.size < obj2.ls.size) yield ((obj1, obj2), Arrow(obj1, obj2))).toMap
      Category(objs, ids ++ arrows)
    }

    private def makeAllObjects(n: Int): List[Obj] = {
      val elements = (0 to n).toList
      var objects = List(Set.empty[Int])
      for (_ <- 1 to n) {
        objects = (objects ++ (objects flatMap (set => elements map (e => set + e)))).distinct
      }
      (objects map (ls => Obj(ls.toList))) ++ List(Obj(elements))
    }
  }

  def compose(als1: List[Arrow], als2: List[Arrow]): List[Arrow] = als1 flatMap (a => a ** als2)

  def showArrows(arrows: List[Arrow]): Unit = {
    arrows foreach { a =>
      println(a.toString)
    }
  }
}