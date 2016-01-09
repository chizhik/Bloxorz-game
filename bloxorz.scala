import org.otfried.cs109.UI._

import java.awt.image.BufferedImage
import java.awt.{Graphics2D,Color,Font,BasicStroke}
import java.awt.geom._

case class Pos(x: Int, y: Int) {
	def dx(d: Int): Pos = Pos(x+d,y)
	def dy(d: Int): Pos = Pos(x,y+d)
	def equal(rhs: Pos): Boolean = {
		if (x == rhs.x && y == rhs.y)
			true
		else 
			false
	}
}

class Block(p: Pos) {
	var poss = List(p)

	override def toString: String = {
		if (isStanding) {
			"Block{"+poss(0).toString+"}"
		} else {
			"Block{"+poss(0).toString+","+poss(1).toString+"}"
		}
	}

	def positions: List[Pos] = poss

	def isStanding: Boolean = {
		if (poss.length == 1)
			true
		else
			false
	}

	def left {
		if (isStanding) {
			poss = List(poss(0).dx(-1),poss(0).dx(-2))
		} else if (poss(0).equal(poss(1).dx(-1))) {
			poss = List(poss(0).dx(-1))
		} else if (poss(1).equal(poss(0).dx(-1))) {
			poss = List(poss(1).dx(-1))
		} else {
			poss = List(poss(0).dx(-1), poss(1).dx(-1))
		}
	}

	def right {
		if (isStanding) {
			poss = List(poss(0).dx(1),poss(0).dx(2))
		} else if (poss(0).equal(poss(1).dx(1))) {
			poss = List(poss(0).dx(1))
		} else if (poss(1).equal(poss(0).dx(1))) {
			poss = List(poss(1).dx(1))
		} else {
			poss = List(poss(0).dx(1), poss(1).dx(1))
		}
	}

	def up {
		if (isStanding) {
			poss = List(poss(0).dy(-1),poss(0).dy(-2))
		} else if (poss(0).equal(poss(1).dy(-1))) {
			poss = List(poss(0).dy(-1))
		} else if (poss(1).equal(poss(0).dy(-1))) {
			poss = List(poss(1).dy(-1))
		} else {
			poss = List(poss(0).dy(-1), poss(1).dy(-1))
		}
	}

	def down {
		if (isStanding) {
			poss = List(poss(0).dy(1),poss(0).dy(2))
		} else if (poss(0).equal(poss(1).dy(1))) {
			poss = List(poss(0).dy(1))
		} else if (poss(1).equal(poss(0).dy(1))) {
			poss = List(poss(1).dy(1))
		} else {
			poss = List(poss(0).dy(1), poss(1).dy(1))
		}
	}
}

class Terrain(fname: String) {
	private val F = scala.io.Source.fromFile(fname)
	private var M = Map[Pos, Int]()
	private var x = 0
	private var y = 0
	private var w = 0
	private var h = 0
	private var s = Pos(0,0)
	private var t = Pos(0,0)
	for (l <- F.getLines()) {
		for (i <- l) {
			if (i == 'o') {
				M = M + (Pos(x,y) -> 2)
				if (x+1 > w) 
					w = x+1
				if (y+1 > h)
					h = y+1
			} else if (i == '.') {
				M = M + (Pos(x,y) -> 1)
				if (x+1 > w) 
					w = x+1
				if (y+1 > h)
					h = y+1
			} else if (i == 'S') {
				s = Pos(x,y)
				M = M + (Pos(x,y) -> 2)
				if (x+1 > w) 
					w = x+1
				if (y+1 > h)
					h = y+1
			} else if (i == 'T') {
				t = Pos(x,y)
				M = M + (Pos(x,y) -> 2)
				if (x+1 > w) 
					w = x+1
				if (y+1 > h)
					h = y+1
			}
			x += 1
		}
		y += 1
		x = 0
	}

	def start: Pos = s
	def target: Pos = t
	def width: Int = w
	def height: Int = h

	def apply(p: Pos): Int = M.getOrElse(p, 0)
	def canHold(b: Block): Boolean = {
		if (b.isStanding) {
			if (apply(b.positions(0)) < 2) 
				return false
			else
				return true
		} else if (apply(b.positions(0)) == 0 || apply(b.positions(1)) == 0) {
		return false
		} else {
			return true
		}
	}
}

object Bloxorz {
	val backgroundColor = new Color(0xbbada0)
	val color1 = new Color(0xeee4da)
	val colorStart = new Color(0xf65e3b)
	val colorEnd = new Color(0xedc22e)

	def tileSize(t: Terrain): Int = {
    	var ts = 60
    	while (ts > 5) {
    	  	if (t.width * ts <= 800 && t.height * ts <= 640)
				return ts
      		ts -= 2
    	}
    	ts
  	}

  	def draw(canvas: BufferedImage, terrain: Terrain, ts: Int, block: Block) {
  		val g = canvas.createGraphics()

  		g.setColor(backgroundColor)
  		g.fillRect(0,0,canvas.getWidth, canvas.getHeight)

  		g.setRenderingHint(java.awt.RenderingHints.KEY_ANTIALIASING, java.awt.RenderingHints.VALUE_ANTIALIAS_ON)

		for (i <- 0 until terrain.width) {
			for (j <- 0 until terrain.height) {
				if (block.positions contains Pos(i,j)) {
					g.setColor(Color.GREEN)
				} else if (terrain.start.equal(Pos(i,j))) {
					g.setColor(colorStart)
				} else if (terrain.target.equal(Pos(i,j))) {
					g.setColor(colorEnd)
				} else if (terrain(Pos(i,j)) == 2) {
					g.setColor(color1)
				} else if (terrain(Pos(i,j)) == 1) {
					g.setColor(Color.BLUE)
				} else {
					g.setColor(backgroundColor)
				}
				g.fill(new Rectangle2D.Double(2.0 + i*(ts+2), 2.0 + j*(ts+2),ts, ts))
			}
		}  		
  	}

  	def makeMove(block: Block, ch: Char) {
  		if (ch == 'r')
  			block.right
  		else if (ch == 'l')
  			block.left
  		else if (ch == 'u')
  			block.up
  		else 
  			block.down
  	}
  
  	def playLevel(level: Int) {
    	val terrain = new Terrain("level%02d.txt".format(level))
    	val ts = tileSize(terrain)
    	val canvas = new BufferedImage((ts+2) * terrain.width + 20, 
					   ts * terrain.height + 20, 
					   BufferedImage.TYPE_INT_RGB)
    	var block = new Block(terrain.start)
    	var moves = 0
    	while (true) {
    	  	setTitle("Bloxorz Level %d (%d moves)".format(level, moves))
      		draw(canvas, terrain, ts, block)
      		show(canvas)
      		val ch = waitKey()
      		if ("lurd" contains ch) {
				makeMove(block, ch)
				moves += 1
      		}
      		if (block.isStanding && block.positions.head == terrain.target) {
				showMessage("Congratulations, you solved level %d".format(level))
				return
      		}
      		if (!terrain.canHold(block)) {
				showMessage("You fell off the terrain")
				block = new Block(terrain.start)
      		}
    	}
  	}

	def main(args: Array[String]) {
		for (i <- 1 to 9) {
			playLevel(i)
		}
	}
}

