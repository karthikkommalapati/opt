from pptx import Presentation
from pptx.util import Inches, Pt, Emu
from pptx.dml.color import RGBColor
from pptx.enum.text import PP_ALIGN, MSO_ANCHOR
from pptx.enum.shapes import MSO_SHAPE
from pptx.oxml.ns import qn
import copy

# ---------- Palette ----------
NAVY = RGBColor(0x1B, 0x2A, 0x4A)
NAVY_LIGHT = RGBColor(0x2E, 0x40, 0x66)
BLUE = RGBColor(0x1F, 0x6F, 0xB2)
BLUE_LIGHT = RGBColor(0xE9, 0xF2, 0xFA)
GREEN = RGBColor(0x1E, 0x7A, 0x4C)
GREEN_LIGHT = RGBColor(0xE8, 0xF5, 0xEC)
RED = RGBColor(0xA6, 0x35, 0x2C)
RED_LIGHT = RGBColor(0xFB, 0xEA, 0xE8)
GREY_TEXT = RGBColor(0x4A, 0x52, 0x5E)
WHITE = RGBColor(0xFF, 0xFF, 0xFF)
BG = RGBColor(0xF7, 0xF8, 0xFA)
TABLE_HEADER = NAVY
TABLE_ALT = RGBColor(0xF0, 0xF3, 0xF7)

prs = Presentation()
prs.slide_width = Inches(13.333)
prs.slide_height = Inches(7.5)
SW, SH = prs.slide_width, prs.slide_height

slide = prs.slides.add_slide(prs.slide_layouts[6])  # blank

def set_bg(slide, color):
    bg = slide.background
    bg.fill.solid()
    bg.fill.fore_color.rgb = color

set_bg(slide, BG)

def add_rect(x, y, w, h, color, line=False, shadow=False, round_=False):
    shape_type = MSO_SHAPE.ROUNDED_RECTANGLE if round_ else MSO_SHAPE.RECTANGLE
    shp = slide.shapes.add_shape(shape_type, x, y, w, h)
    shp.fill.solid()
    shp.fill.fore_color.rgb = color
    if not line:
        shp.line.fill.background()
    else:
        shp.line.color.rgb = line
        shp.line.width = Pt(0.75)
    shp.shadow.inherit = False
    if round_:
        try:
            shp.adjustments[0] = 0.06
        except Exception:
            pass
    return shp

def add_text(x, y, w, h, text, size=14, color=GREY_TEXT, bold=False, align=PP_ALIGN.LEFT,
             anchor=MSO_ANCHOR.TOP, font="Calibri", italic=False, line_spacing=1.0):
    tb = slide.shapes.add_textbox(x, y, w, h)
    tf = tb.text_frame
    tf.word_wrap = True
    tf.vertical_anchor = anchor
    tf.margin_left = 0
    tf.margin_right = 0
    tf.margin_top = 0
    tf.margin_bottom = 0
    p = tf.paragraphs[0]
    p.alignment = align
    p.line_spacing = line_spacing
    r = p.add_run()
    r.text = text
    r.font.size = Pt(size)
    r.font.bold = bold
    r.font.italic = italic
    r.font.color.rgb = color
    r.font.name = font
    return tb

def add_bullets(x, y, w, h, items, size=12.5, color=GREY_TEXT, bold_first=False,
                 bullet_color=None, font="Calibri", space_after=6, line_spacing=1.05):
    tb = slide.shapes.add_textbox(x, y, w, h)
    tf = tb.text_frame
    tf.word_wrap = True
    tf.margin_left = 0
    tf.margin_right = 0
    tf.margin_top = 0
    tf.margin_bottom = 0
    for i, item in enumerate(items):
        p = tf.paragraphs[0] if i == 0 else tf.add_paragraph()
        p.space_after = Pt(space_after)
        p.line_spacing = line_spacing
        r = p.add_run()
        r.text = "•  " + item
        r.font.size = Pt(size)
        r.font.color.rgb = color
        r.font.name = font
    return tb

# ---------- Header band ----------
add_rect(0, 0, SW, Inches(1.05), NAVY)
add_text(Inches(0.45), Inches(0.10), Inches(9.5), Inches(0.5),
          "From Time-Based to Trigger-Based Scheduling", size=25, color=WHITE, bold=True)
add_text(Inches(0.45), Inches(0.60), Inches(10.5), Inches(0.4),
          "Automated, data-driven job starts — fewer failures, earlier delivery, incidents avoided",
          size=13, color=RGBColor(0xC9, 0xD6, 0xE8), italic=True)

# small accent strip
add_rect(0, Inches(1.05), SW, Inches(0.05), BLUE)

# ---------- Two comparison cards ----------
card_y = Inches(1.30)
card_h = Inches(2.05)
card_w = Inches(6.05)
gap = Inches(0.25)
left_x = Inches(0.45)
right_x = left_x + card_w + gap

# Previous card
add_rect(left_x, card_y, card_w, card_h, RED_LIGHT, round_=True)
add_rect(left_x, card_y, Inches(0.09), card_h, RED)
add_text(left_x + Inches(0.3), card_y + Inches(0.12), card_w - Inches(0.5), Inches(0.35),
          "PREVIOUS — Time-Based", size=15, color=RED, bold=True)
add_bullets(left_x + Inches(0.3), card_y + Inches(0.52), card_w - Inches(0.55), Inches(1.45), [
    "Jobs fixed to run at a set clock time, regardless of data readiness",
    "If upstream data hadn't arrived yet, the job still fired — and failed",
    "Every failure created a production incident, even when data simply arrived late",
], size=11, color=NAVY, space_after=5)

# Current card
add_rect(right_x, card_y, card_w, card_h, GREEN_LIGHT, round_=True)
add_rect(right_x, card_y, Inches(0.09), card_h, GREEN)
add_text(right_x + Inches(0.3), card_y + Inches(0.12), card_w - Inches(0.5), Inches(0.35),
          "CURRENT — Trigger-Based", size=15, color=GREEN, bold=True)
add_bullets(right_x + Inches(0.3), card_y + Inches(0.52), card_w - Inches(0.55), Inches(1.45), [
    "Jobs start automatically the moment upstream data actually arrives",
    "No fixed clock time — no more “fired too early, no data” failures",
    "Runs as early as data allows, and skips the incident entirely when the old fixed time would have been missed",
    "Absorbs upstream delays cleanly — old time-bound configs meant a delay caused two days' data to land and run together, needing L2 to manually split files by date and reprocess; across multiple mandates this became a massive manual effort",
], size=11, color=NAVY, space_after=4)

# ---------- KPI strip ----------
kpi_y = card_y + card_h + Inches(0.20)
kpi_h = Inches(0.85)
kpi_w = (SW - Inches(0.9) - Inches(0.6)) / 4
kpi_labels = [
    ("Avg. Early Start", "2h 28m earlier", BLUE),
    ("Avg. Upstream Delay Reduced", "1h 53m faster", BLUE),
    ("Prior Monthly Incidents (avg)", "55 / month, all mandates", RED),
    ("Incidents Avoided", "27 across 6 mandates", GREEN),
]
for i, (label, value, color) in enumerate(kpi_labels):
    x = Inches(0.45) + i * (kpi_w + Inches(0.2))
    add_rect(x, kpi_y, kpi_w, kpi_h, WHITE, line=RGBColor(0xDD, 0xE2, 0xEA), round_=True)
    add_text(x + Inches(0.2), kpi_y + Inches(0.09), kpi_w - Inches(0.36), Inches(0.36),
              label.upper(), size=9, color=GREY_TEXT, bold=True, line_spacing=1.05)
    add_text(x + Inches(0.2), kpi_y + Inches(0.46), kpi_w - Inches(0.36), Inches(0.36),
              value, size=14, color=color, bold=True)

# ---------- Table ----------
tbl_y = kpi_y + kpi_h + Inches(0.22)
tbl_h = SH - tbl_y - Inches(0.28)
tbl_w = SW - Inches(0.9)
tbl_x = Inches(0.45)

headers = ["Mandate", "Previous\nStart Time", "New Automated\nStart Time",
           "Avg. Improvement\n(Early)", "Avg. Upstream\nDelay",
           "Incidents Occurred\n(Late Arrival /\nOut-of-Time-Bound)", "Incidents\nAvoided"]
rows = [
    ["MND-EQ-01 (Equities)", "06:45", "04:10", "2h 35m", "1h 50m", "9", "4"],
    ["MND-FI-02 (Fixed Income)", "07:15", "05:00", "2h 15m", "2h 05m", "12", "6"],
    ["MND-FX-03 (FX)", "06:30", "04:45", "1h 45m", "1h 20m", "6", "2"],
    ["MND-CR-04 (Credit)", "07:00", "04:30", "2h 30m", "1h 55m", "10", "5"],
    ["MND-EQ-05 (Equities)", "06:50", "03:55", "2h 55m", "2h 10m", "8", "3"],
    ["MND-MM-06 (Money Mkt)", "07:10", "04:20", "2h 50m", "2h 00m", "10", "7"],
]

n_rows = len(rows) + 1
n_cols = len(headers)
gshape = slide.shapes.add_table(n_rows, n_cols, tbl_x, tbl_y, tbl_w, tbl_h)
table = gshape.table

col_weights = [0.20, 0.10, 0.10, 0.13, 0.13, 0.17, 0.17]
total_w = tbl_w
for i, w in enumerate(col_weights):
    table.columns[i].width = Emu(int(total_w * w))

# header row
table.rows[0].height = Inches(0.62)
for c, htext in enumerate(headers):
    cell = table.cell(0, c)
    cell.fill.solid()
    cell.fill.fore_color.rgb = TABLE_HEADER
    cell.vertical_anchor = MSO_ANCHOR.MIDDLE
    cell.margin_left = Inches(0.08)
    cell.margin_right = Inches(0.08)
    cell.margin_top = Inches(0.02)
    cell.margin_bottom = Inches(0.02)
    tf = cell.text_frame
    tf.word_wrap = True
    lines = htext.split("\n")
    for li, line in enumerate(lines):
        p = tf.paragraphs[0] if li == 0 else tf.add_paragraph()
        p.alignment = PP_ALIGN.CENTER
        r = p.add_run()
        r.text = line
        r.font.size = Pt(9.5)
        r.font.bold = True
        r.font.color.rgb = WHITE
        r.font.name = "Calibri"

row_h = (tbl_h - Inches(0.62)) / len(rows)
for ri, row in enumerate(rows, start=1):
    table.rows[ri].height = int(row_h)
    for ci, val in enumerate(row):
        cell = table.cell(ri, ci)
        cell.fill.solid()
        cell.fill.fore_color.rgb = TABLE_ALT if ri % 2 == 0 else WHITE
        cell.vertical_anchor = MSO_ANCHOR.MIDDLE
        cell.margin_left = Inches(0.08)
        cell.margin_right = Inches(0.08)
        cell.margin_top = Inches(0.02)
        cell.margin_bottom = Inches(0.02)
        tf = cell.text_frame
        tf.word_wrap = True
        p = tf.paragraphs[0]
        p.alignment = PP_ALIGN.CENTER if ci != 0 else PP_ALIGN.LEFT
        r = p.add_run()
        r.text = val
        r.font.size = Pt(11)
        r.font.color.rgb = NAVY
        r.font.name = "Calibri"
        r.font.bold = (ci == 0)
        if ci == 5:
            r.font.color.rgb = RED
            r.font.bold = True
        if ci == 6:
            r.font.color.rgb = GREEN
            r.font.bold = True

# remove default table style banding via style id (keep clean look)
tbl_el = table._tbl
style_el = tbl_el.find(qn('a:tblPr'))
if style_el is not None:
    style_el.set('firstRow', '0')
    style_el.set('bandRow', '0')

out_path = "Trigger_Based_vs_Time_Based.pptx"
prs.save(out_path)
print("Saved:", out_path)
