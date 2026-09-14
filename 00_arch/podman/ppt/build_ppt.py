"""
Generates Trigger_Based_vs_Time_Based.pptx from Trigger_Based_vs_Time_Based.html.

Single source of truth = the HTML file. Edit the HTML (title, card bullets,
KPI tiles, table title/headers/rows), then re-run this script. Do not hand-edit
the pptx content directly, or the two will drift apart again.

Usage:
    pip install python-pptx beautifulsoup4
    python3 build_ppt.py
"""

from pathlib import Path

from bs4 import BeautifulSoup
from pptx import Presentation
from pptx.util import Inches, Pt, Emu
from pptx.dml.color import RGBColor
from pptx.enum.text import PP_ALIGN, MSO_ANCHOR
from pptx.enum.shapes import MSO_SHAPE
from pptx.oxml.ns import qn

HERE = Path(__file__).resolve().parent
HTML_PATH = HERE / "Trigger_Based_vs_Time_Based.html"
OUT_PATH = HERE / "Trigger_Based_vs_Time_Based.pptx"

# ---------- Palette (mirrors the HTML's :root light-theme tokens) ----------
NAVY = RGBColor(0x1B, 0x2A, 0x4A)
NAVY_LIGHT = RGBColor(0x2E, 0x40, 0x66)
BLUE = RGBColor(0x1F, 0x6F, 0xB2)
GREEN = RGBColor(0x1E, 0x7A, 0x4C)
GREEN_LIGHT = RGBColor(0xE8, 0xF5, 0xEC)
RED = RGBColor(0xA6, 0x35, 0x2C)
RED_LIGHT = RGBColor(0xFB, 0xEA, 0xE8)
GREY_TEXT = RGBColor(0x4A, 0x52, 0x5E)
WHITE = RGBColor(0xFF, 0xFF, 0xFF)
BG = RGBColor(0xF7, 0xF8, 0xFA)
TABLE_HEADER = NAVY
TABLE_ALT = RGBColor(0xF0, 0xF3, 0xF7)


# ---------- HTML parsing ----------
def text_of(el):
    """Flatten an element's text, treating <br> as a newline and collapsing
    incidental whitespace on either side of it."""
    if el is None:
        return ""
    parts = []
    for node in el.contents:
        if getattr(node, "name", None) == "br":
            parts.append("\n")
        else:
            parts.append(node.get_text() if hasattr(node, "get_text") else str(node))
    return "".join(parts).strip()


def flat_text(el):
    """Single-line text, collapsing all internal whitespace."""
    if el is None:
        return ""
    return " ".join(el.get_text(separator=" ", strip=True).split())


def parse_slide(html_path):
    soup = BeautifulSoup(html_path.read_text(encoding="utf-8"), "html.parser")

    title = flat_text(soup.select_one(".slide-header h1"))
    subtitle = flat_text(soup.select_one(".slide-header p"))

    previous_bullets = [flat_text(li) for li in soup.select(".card.previous li")]
    current_bullets = [flat_text(li) for li in soup.select(".card.current li")]

    kpis = []
    for kpi in soup.select(".kpi"):
        classes = kpi.get("class", [])
        color = RED if "red" in classes else GREEN if "green" in classes else BLUE
        kpis.append({
            "label": flat_text(kpi.select_one(".label")),
            "value": flat_text(kpi.select_one(".value")),
            "color": color,
        })

    table_title = flat_text(soup.select_one(".table-title"))

    header_cells = soup.select("table thead th")
    headers = [text_of(th) for th in header_cells]

    rows = []
    for tr in soup.select("table tbody tr"):
        cells = []
        for td in tr.select("td"):
            classes = td.get("class", [])
            color = GREEN if "avoided" in classes else RED if "occurred" in classes else None
            cells.append({"text": flat_text(td), "color": color})
        rows.append(cells)

    return {
        "title": title,
        "subtitle": subtitle,
        "previous_bullets": previous_bullets,
        "current_bullets": current_bullets,
        "kpis": kpis,
        "table_title": table_title,
        "headers": headers,
        "rows": rows,
    }


# ---------- pptx drawing helpers ----------
def set_bg(slide, color):
    bg = slide.background
    bg.fill.solid()
    bg.fill.fore_color.rgb = color


def add_rect(slide, x, y, w, h, color, line=False, round_=False):
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


def add_text(slide, x, y, w, h, text, size=14, color=GREY_TEXT, bold=False,
             align=PP_ALIGN.LEFT, anchor=MSO_ANCHOR.TOP, font="Calibri",
             italic=False, line_spacing=1.0):
    tb = slide.shapes.add_textbox(x, y, w, h)
    tf = tb.text_frame
    tf.word_wrap = True
    tf.vertical_anchor = anchor
    tf.margin_left = tf.margin_right = tf.margin_top = tf.margin_bottom = 0
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


def add_bullets(slide, x, y, w, h, items, size=11, color=NAVY, font="Calibri", space_after=4):
    tb = slide.shapes.add_textbox(x, y, w, h)
    tf = tb.text_frame
    tf.word_wrap = True
    tf.margin_left = tf.margin_right = tf.margin_top = tf.margin_bottom = 0
    for i, item in enumerate(items):
        p = tf.paragraphs[0] if i == 0 else tf.add_paragraph()
        p.space_after = Pt(space_after)
        p.line_spacing = 1.05
        r = p.add_run()
        r.text = "•  " + item
        r.font.size = Pt(size)
        r.font.color.rgb = color
        r.font.name = font
    return tb


# ---------- Build ----------
def build(data):
    prs = Presentation()
    prs.slide_width = Inches(13.333)
    prs.slide_height = Inches(7.5)
    SW, SH = prs.slide_width, prs.slide_height

    slide = prs.slides.add_slide(prs.slide_layouts[6])
    set_bg(slide, BG)

    # Header band
    add_rect(slide, 0, 0, SW, Inches(1.05), NAVY)
    add_text(slide, Inches(0.45), Inches(0.10), Inches(10.5), Inches(0.5),
              data["title"], size=25, color=WHITE, bold=True)
    add_text(slide, Inches(0.45), Inches(0.60), Inches(11.0), Inches(0.4),
              data["subtitle"], size=13, color=RGBColor(0xC9, 0xD6, 0xE8), italic=True)
    add_rect(slide, 0, Inches(1.05), SW, Inches(0.05), BLUE)

    # Comparison cards
    card_y = Inches(1.30)
    max_bullets = max(len(data["previous_bullets"]), len(data["current_bullets"]), 1)
    card_h = Inches(0.65 + 0.36 * max_bullets)
    card_w = Inches(6.05)
    gap = Inches(0.25)
    left_x = Inches(0.45)
    right_x = left_x + card_w + gap

    add_rect(slide, left_x, card_y, card_w, card_h, RED_LIGHT, round_=True)
    add_rect(slide, left_x, card_y, Inches(0.09), card_h, RED)
    add_text(slide, left_x + Inches(0.3), card_y + Inches(0.12), card_w - Inches(0.5), Inches(0.35),
              "PREVIOUS — Time-Based", size=15, color=RED, bold=True)
    add_bullets(slide, left_x + Inches(0.3), card_y + Inches(0.52), card_w - Inches(0.55),
                card_h - Inches(0.6), data["previous_bullets"])

    add_rect(slide, right_x, card_y, card_w, card_h, GREEN_LIGHT, round_=True)
    add_rect(slide, right_x, card_y, Inches(0.09), card_h, GREEN)
    add_text(slide, right_x + Inches(0.3), card_y + Inches(0.12), card_w - Inches(0.5), Inches(0.35),
              "CURRENT — Trigger-Based", size=15, color=GREEN, bold=True)
    add_bullets(slide, right_x + Inches(0.3), card_y + Inches(0.52), card_w - Inches(0.55),
                card_h - Inches(0.6), data["current_bullets"])

    # KPI strip
    kpi_y = card_y + card_h + Inches(0.20)
    kpi_h = Inches(0.85)
    n_kpis = max(len(data["kpis"]), 1)
    kpi_gap = Inches(0.2)
    kpi_w = (SW - Inches(0.9) - kpi_gap * (n_kpis - 1)) / n_kpis
    for i, kpi in enumerate(data["kpis"]):
        x = Inches(0.45) + i * (kpi_w + kpi_gap)
        add_rect(slide, x, kpi_y, kpi_w, kpi_h, WHITE, line=RGBColor(0xDD, 0xE2, 0xEA), round_=True)
        add_text(slide, x + Inches(0.2), kpi_y + Inches(0.09), kpi_w - Inches(0.36), Inches(0.36),
                  kpi["label"].upper(), size=9, color=GREY_TEXT, bold=True, line_spacing=1.05)
        add_text(slide, x + Inches(0.2), kpi_y + Inches(0.46), kpi_w - Inches(0.36), Inches(0.36),
                  kpi["value"], size=14, color=kpi["color"], bold=True)

    # Table title
    add_text(slide, Inches(0.45), kpi_y + kpi_h + Inches(0.14), Inches(9.0), Inches(0.3),
              data["table_title"], size=13, color=NAVY, bold=True)

    # Table
    tbl_y = kpi_y + kpi_h + Inches(0.46)
    tbl_h = SH - tbl_y - Inches(0.28)
    tbl_w = SW - Inches(0.9)
    tbl_x = Inches(0.45)

    headers = data["headers"]
    rows = data["rows"]
    n_cols = len(headers)
    n_rows = len(rows) + 1

    gshape = slide.shapes.add_table(n_rows, n_cols, tbl_x, tbl_y, tbl_w, tbl_h)
    table = gshape.table

    # Column widths: size each column by the widest content it holds
    # (header's longest line, or the widest cell value in that column).
    col_metrics = []
    for c in range(n_cols):
        header_lines = headers[c].split("\n")
        max_len = max((len(line) for line in header_lines), default=1)
        for row in rows:
            if c < len(row):
                max_len = max(max_len, len(row[c]["text"]))
        col_metrics.append(max(max_len, 3))
    total_metric = sum(col_metrics)
    col_weights = [m / total_metric for m in col_metrics]
    for i, w in enumerate(col_weights):
        table.columns[i].width = Emu(int(tbl_w * w))

    header_row_lines = max((len(h.split("\n")) for h in headers), default=1)
    header_h = Inches(0.28 + 0.16 * header_row_lines)
    table.rows[0].height = header_h
    for c, htext in enumerate(headers):
        cell = table.cell(0, c)
        cell.fill.solid()
        cell.fill.fore_color.rgb = TABLE_HEADER
        cell.vertical_anchor = MSO_ANCHOR.MIDDLE
        cell.margin_left = cell.margin_right = Inches(0.08)
        cell.margin_top = cell.margin_bottom = Inches(0.02)
        tf = cell.text_frame
        tf.word_wrap = True
        for li, line in enumerate(htext.split("\n")):
            p = tf.paragraphs[0] if li == 0 else tf.add_paragraph()
            p.alignment = PP_ALIGN.LEFT if c == 0 else PP_ALIGN.CENTER
            r = p.add_run()
            r.text = line
            r.font.size = Pt(9.5)
            r.font.bold = True
            r.font.color.rgb = WHITE
            r.font.name = "Calibri"

    row_h = int((tbl_h - header_h) / max(len(rows), 1))
    for ri, row in enumerate(rows, start=1):
        table.rows[ri].height = row_h
        for ci, cell_data in enumerate(row):
            cell = table.cell(ri, ci)
            cell.fill.solid()
            cell.fill.fore_color.rgb = TABLE_ALT if ri % 2 == 0 else WHITE
            cell.vertical_anchor = MSO_ANCHOR.MIDDLE
            cell.margin_left = cell.margin_right = Inches(0.08)
            cell.margin_top = cell.margin_bottom = Inches(0.02)
            tf = cell.text_frame
            tf.word_wrap = True
            p = tf.paragraphs[0]
            p.alignment = PP_ALIGN.LEFT if ci == 0 else PP_ALIGN.CENTER
            r = p.add_run()
            r.text = cell_data["text"]
            r.font.size = Pt(11)
            r.font.name = "Calibri"
            if ci == 0:
                r.font.bold = True
                r.font.color.rgb = NAVY
            elif cell_data["color"] is not None:
                r.font.bold = True
                r.font.color.rgb = cell_data["color"]
            else:
                r.font.color.rgb = NAVY

    tbl_el = table._tbl
    style_el = tbl_el.find(qn("a:tblPr"))
    if style_el is not None:
        style_el.set("firstRow", "0")
        style_el.set("bandRow", "0")

    return prs


def main():
    data = parse_slide(HTML_PATH)
    prs = build(data)
    prs.save(str(OUT_PATH))
    print("Saved:", OUT_PATH)


if __name__ == "__main__":
    main()
