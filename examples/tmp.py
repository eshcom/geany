from __future__ import absolute_import

[print(f'{"="*120}\n{job}') for job in db.filter(Jobs)]
[print('\n'.join(f'{"="*120}\n{job}'.split('\n')[0:5])) for job in db.filter(Jobs)]
[print(f'{"="*120}\n{task}') for task in db.filter(Tasks)]
[print('\n'.join(f'{"="*120}\n{job}'.split('\n')[0:12])) for job in db.filter(Tasks)]
[print(f'{"="*150}\n{task}') for task in db.exe_stack()]

    def __repr__(self):
        d = OrderedDict(
            [ ('Последний PID', self.pidnum), ('Заблокирован', self.is_blocked),
              ('Путь к PID', self.abd_pid), ('Путь к сокету', self.abd_sock),
              ('Настраиваемые параметры:', f'{"+"*80}'),
              (f'ТПС включено\t{"-"*8}\trabbit_use', self.rabbit_use),
              (f'Файл работ\t{"-"*8}\tjobs_json', self.jobs_json),
              (f'Отчёты\t\t{"-"*8}\tlogpath\t', self.logpath),
              (f'Krb principal\t{"-"*8}\tkrb_principal', self.krb_principal),
              (f'Очередь IN\t{"-"*8}\trabbit_listen', self.rabbit_listen),
              (f'Имя сервиса ТПС\t{"-"*8}\trabbit_name', self.rabbit_name),
              (f'Имя хоста ТПС\t{"-"*8}\trabbit_server', self.rabbit_server),
              (f'Номер порта ТПС\t{"-"*8}\trabbit_port', self.rabbit_port),
              (f'Очередь ответа\t{"-"*8}\trabbit_send', self.rabbit_send)])
        return '\n'.join([f'{k}\t{"-"*8}\t{v}' for k, v in d.items()])

message = f"""
			Hi {name}. 
			You are a {profession}. 
			You were in {affiliation}.
		   """
test = f"{'Eric Idle'}"
test = f'{"Eric Idle"}'
test = f"""Eric Idle"""
test = f'''Eric Idle'''
test = f"{{{70 + 4}}}"
test = f"The \"comedian\" is {name}, aged {age}."

print("tes%t %s%%\n%%%d\n%5d%.7d\n%5.7d\n%5.*d\n%-+05d\n%%s")

TEST1 = "\ \sx\tx\nx\rxx\"x\'x\!x\@x\#x\$x\%x\6x"
TEST2 = '\ \sx\tx\nx\rxx\"x\'x\!x\@x\#x\$x\%x\6x'
TEST3 = "\u00d0x\u00a8x\u00d0x\u00b0x\u00d0x\u00b1x\u00d0x\u00bbx\u00d0"
TEST4 = '\u00d0x\u00a8x\u00d0x\u00b0x\u00d0x\u00b1x\u00d0x\u00bbx\u00d0'

TEST5 = f"part1 \
		  part2 \
		  part3"

TEST5 = f"part1 \
		  part2 \
		  part3"

TEST5 = f"part1 \n\
		  part2 \n\
		  part3"

TEST5 = "part1 \n\
		 part2 \n\
		 part3"

TEST5 = "part1 \n
		 part2 \n\
		 part3"

TEST5 = "part1 \n\
		 part2 \n
		 part3"

TEST5 = "part1 \
		 part2 \
		 part3"

TEST5 = "part1 \	
		 part2 \
		 part3"

TEST5 = "part1 \		
		 part2 \
		 part3"

TEST5 = "part1 \
		 part2 \	
		 part3"

TEST5 = "part1 \
		 part2 \		
		 part3"

TEST5 = "part1 \	
		 part2 \	
		 part3"

TEST5 = "part1 
		 part2 
		 part3"

TEST5 = "part1 
		 part2"


if True:
	ch = "t\"
	ch = 't\'
	
	ch = "t\""
	ch = 't\''
	
	ch = "t\n"
	ch = 't\n'
	
	ch = "t\r"
	ch = 't\r'
	
	ch = "t\n\";
	ch = 't\n\';
	
	ch = "t\n\"
	ch = 't\n\'
	
	ch = "t\n\
	---
	ch = 't\n\
	---
	
	ch = "t\
	---
	ch = 't\
	---
	
	ch = "t\n\r"
	ch = 't\n\r'
	
	ch = "t\nr"
	ch = 't\nr'
	
	ch = "\377"
	ch = '\377'
	
	ch = "test"
	ch = 'test'
	
	ch = "test"
		 "test"
	ch = 'test'
		 'test'
	
	ch = "test""test"
	ch = 'test''test'
	
	ch = "test"t"test"
	ch = 'test't'test'
	
	ch = "test
	ch = 'test
	
	ch = test";"
	ch = test';'
	
	ch = test"
	ch = test'
	
	ch = "5.33"
	ch = '5.33'
	
	ch = "5.33'
	ch = '5.33"
	
	ch = "5.33
	ch = '5.33
	
	ch = "5.33	
	ch = '5.33	
	
	ch = "5.33
	ch = '5.33
	
	ch = "5.33     \
t			\ntest"
	ch = '5.33     \
t			\ntest'
	
	ch = "5.33    n\
t			\ntest"
	ch = '5.33    n\
t			\ntest'
	
	ch = "5.33   \n\
t			\ntest"
	ch = '5.33   \n\
t			\ntest'
	
	ch = "5.33   \\\
t			\ntest"
	ch = '5.33   \\\
t			\ntest'
	
	ch = "5.33  \nt\
t			\ntest"
	ch = '5.33  \nt\
t			\ntest'
	
	ch = "5.33   \n\
t			\ntest"
	ch = '5.33   \n\
t			\ntest'
	
	ch = "5.33   \n
t			\ntest"
	ch = '5.33   \n
t			\ntest'
	
	ch = "5.33     \	
t			\ntest"
	ch = '5.33     \	
t			\ntest'
	
	ch = "5.33     \		
t			\ntest"
	ch = '5.33     \		
t			\ntest'
	
	ch = "5.33     \
t			\ntest \
t			\ntest"
	ch = '5.33     \
t			\ntest \
t			\ntest'
	
	ch = "5.33 
t			\ntest \
t			\ntest"
	ch = '5.33 
t			\ntest \
t			\ntest'
	
	ch = "5.33     \
t			\ntest 
t			\ntest"
	ch = '5.33     \
t			\ntest 
t			\ntest'
	
	ch = "5.33     \
t			test 
t			test"
	ch = '5.33     \
t			test 
t			test'
	
	ch = "5.33     \
t			test\n 
t			test\n"
	ch = '5.33     \
t			test\n 
t			test\n'
	
	ch = "5.33   \"
t			\ntest"
	ch = '5.33   \'
t			\ntest'
	
	ch = "5.33   \"
t			"\ntest"
	ch = '5.33   \'
t			'\ntest'
	
	ch = "5.33 
t			\ntest"
	ch = '5.33 
t			\ntest'


__copyright__ = "Copyright 2005 Canonical Ltd."
__author__    = "Scott James Remnant <scott@ubuntu.com>"

import math

import gtk
import pango
import cairo

try:
	from gi.repository import GObject as gobject
except ImportError:
	import gobject

# Styles used when rendering revision graph edges
style_SOLID = 0
style_DASHED = 1

class CellRendererGraph(gtk.GenericCellRenderer):
	"""Cell renderer for directed graph.
	"""
	columns_len = 0
	__gproperties__ = {"graph": (gobject.TYPE_PYOBJECT, "graph",
								 "revision node instruction",
								 gobject.PARAM_WRITABLE)}

	def do_set_property(self, property, value):
		"""Set properties from GObject properties."""
		if not value:
			return
		if property.name == "graph":
			(self.node, self.in_lines, self.out_lines) = value
		else:
			raise AttributeError("no such property: '%s'" % property.name)

	def box_size(self, widget):
		"""Calculate box size based on widget's font.
		"""
		try:
			return self._box_size
		except AttributeError:
			pango_ctx = widget.get_pango_context()
			font_desc = widget.get_style().font_desc
			metrics = pango_ctx.get_metrics(font_desc)
			ascent = pango.PIXELS(metrics.get_ascent())
			descent = pango.PIXELS(metrics.get_descent())
			self._box_size = ascent + descent + 1
			return self._box_size

	def set_colour(self, ctx, colour, bg, fg):
		"""Set the context source colour.
		"""
		if isinstance(colour, str):
			r, g, b = colour[1:3], colour[3:5], colour[5:7]
			colour_rgb = int(r, 16) / 255., int(g, 16) / 255., int(b, 16) / 255.
		else:
			if colour == 0:
				colour_rgb = gtklib.MAINLINE_COLOR
			else:
				colour_rgb = gtklib.LINE_COLORS[colour % len(gtklib.LINE_COLORS)]
		red   = (colour_rgb[0] * fg) or bg
		green = (colour_rgb[1] * fg) or bg
		blue  = (colour_rgb[2] * fg) or bg
		ctx.set_source_rgb(red, green, blue)

	def on_get_size(self, widget, cell_area):
		"""Return the size we need for this cell.
		"""
		box_size = self.box_size(widget) + 1
		width = box_size * (self.columns_len + 1)
		height = box_size
		# FIXME I have no idea how to use cell_area properly
		return (0, 0, width, height)

	def on_render(self, window, widget, bg_area, cell_area, exp_area, flags):
		"""Render an individual cell.
		"""
		ctx = window.cairo_create()
		ctx.rectangle(bg_area.x, bg_area.y, bg_area.width, bg_area.height)
		ctx.clip()
		box_size = self.box_size(widget)

		# Maybe draw branch head highlight under revision node
		if self.node:
			(column, colour) = self.node
			arc_start_position_x = cell_area.x + box_size * column + box_size / 2;
			arc_start_position_y = cell_area.y + cell_area.height / 2;

		ctx.set_line_width(box_size / 8)
		ctx.set_line_cap(cairo.LINE_CAP_ROUND)
		
		# Draw lines into the cell
		if self.in_lines:
			for start, end, lcolour in self.in_lines:
				style = style_SOLID
				self.render_line (ctx, cell_area, box_size,
							 bg_area.y, bg_area.height,
							 start, end, lcolour, style)
		# Draw lines out of the cell
		if self.out_lines:
			for start, end, lcolour in self.out_lines:
				style = style_SOLID
				self.render_line (ctx, cell_area, box_size,
							 bg_area.y + bg_area.height, bg_area.height,
							 start, end, lcolour, style)
		# Draw the revision node in the right column
		if not self.node:
			return
		ctx.arc(arc_start_position_x, arc_start_position_y,
					box_size / 5, 0, 2 * math.pi)
		self.set_colour(ctx, colour, 0.0, 0.5)
		ctx.stroke_preserve()
		self.set_colour(ctx, colour, 0.5, 1.0)
		ctx.fill()

	def render_line (self, ctx, cell_area, box_size, mid,
					 height, start, end, colour, style):
		if start is None:
			x = cell_area.x + box_size * end + box_size / 2
			ctx.move_to(x, mid + height / 3)
			ctx.line_to(x, mid + height / 3)
			ctx.move_to(x, mid + height / 6)
			ctx.line_to(x, mid + height / 6)
		elif end is None:
			x = cell_area.x + box_size * start + box_size / 2
			ctx.move_to(x, mid - height / 3)
			ctx.line_to(x, mid - height / 3)
			ctx.move_to(x, mid - height / 6)
			ctx.line_to(x, mid - height / 6)
		else:
			startx = cell_area.x + box_size * start + box_size / 2
			endx = cell_area.x + box_size * end + box_size / 2
			ctx.move_to(startx, mid - height / 2)

			if start - end == 0 :
				ctx.line_to(endx, mid + height / 2)
			else:
				ctx.curve_to(startx, mid - height / 5,
							 startx, mid - height / 5,
							 startx + (endx - startx) / 2, mid)
				ctx.curve_to(endx, mid + height / 5,
							 endx, mid + height / 5 ,
							 endx, mid + height / 2)
		self.set_colour(ctx, colour, 0.0, 0.65)
		if style == style_DASHED:
			dashes = [1, 2]
			ctx.set_dash(dashes)
		ctx.stroke()
		ctx.set_dash([])
