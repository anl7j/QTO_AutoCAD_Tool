#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
AutoCAD DXF Quantity Takeoff (QTO) - أداة حصر الكميات لملفات الأوتوكاد المستقلة
تقوم بقراءة ملف DXF وحساب:
- مجموع أطوال الخطوط والمسارات (Lines, Polylines, Arcs) حسب الطبقة
- مساحات التهشيرات (Hatches) والدوائر والأشكال المغلقة
- أعداد البلوكات والرموز (Blocks) حسب الاسم والطبقة
- تصدير النتائج تلقائياً إلى تقرير CSV متوافق مع Excel
"""

import sys
import os
import math
import csv
from collections import defaultdict
from typing import Sequence, Tuple, Union

Point2D = Union[Tuple[float, float], Sequence[float]]

try:
    import ezdxf
except ImportError:
    ezdxf = None

def distance_2d(p1: Point2D, p2: Point2D) -> float:
    return math.sqrt((p2[0] - p1[0])**2 + (p2[1] - p1[1])**2)

def polygon_area(points: Sequence[Point2D]) -> float:
    """حساب مساحة مضلع ثنائي الأبعاد باستخدام صيغة Shoelace"""
    if len(points) < 3:
        return 0.0
    area = 0.0
    n = len(points)
    for i in range(n):
        j = (i + 1) % n
        area += points[i][0] * points[j][1]
        area -= points[j][0] * points[i][1]
    return abs(area) / 2.0

def process_dxf(filepath: str):
    if ezdxf is None:
        print("[!] مكتبة ezdxf غير مثبتة. يمكنك تثبيتها عبر: pip install ezdxf")
        return None

    if not os.path.exists(filepath):
        print(f"[!] الملف غير موجود: {filepath}")
        return None

    print(f"[*] جاري قراءة الملف: {filepath} ...")
    doc = ezdxf.readfile(filepath)
    msp = doc.modelspace()

    lengths_by_layer = defaultdict(float)
    areas_by_layer = defaultdict(float)
    blocks_count = defaultdict(int)

    for entity in msp:
        layer = entity.dxf.layer
        dxftype = entity.dxftype()

        # 1. الخطوط المستقيمة (LINE)
        if dxftype == 'LINE':
            start = entity.dxf.start
            end = entity.dxf.end
            length = distance_2d(start, end)
            lengths_by_layer[layer] += length

        # 2. الخطوط المتعددة (LWPOLYLINE)
        elif dxftype == 'LWPOLYLINE':
            points = [(p[0], p[1]) for p in entity.get_points('xy')]
            total_len = 0.0
            for i in range(len(points) - 1):
                total_len += distance_2d(points[i], points[i+1])
            if entity.is_closed and len(points) > 1:
                total_len += distance_2d(points[-1], points[0])
                # حساب المساحة إذا كان مغلقاً
                poly_area = polygon_area(points)
                if poly_area > 0:
                    areas_by_layer[layer] += poly_area
            lengths_by_layer[layer] += total_len

        # 3. الأقواس والدوائر (ARC, CIRCLE)
        elif dxftype == 'CIRCLE':
            r = entity.dxf.radius
            lengths_by_layer[layer] += 2 * math.pi * r
            areas_by_layer[layer] += math.pi * (r ** 2)

        elif dxftype == 'ARC':
            r = entity.dxf.radius
            start_angle = math.radians(entity.dxf.start_angle)
            end_angle = math.radians(entity.dxf.end_angle)
            if end_angle < start_angle:
                end_angle += 2 * math.pi
            arc_len = r * (end_angle - start_angle)
            lengths_by_layer[layer] += arc_len

        # 4. التهشير (HATCH)
        elif dxftype == 'HATCH':
            # محاولة قراءة أو تقدير مساحة مسارات الحدود
            try:
                for path in entity.paths:
                    if hasattr(path, 'vertices') and path.vertices:
                        pts = [(v[0], v[1]) for v in path.vertices]
                        areas_by_layer[layer] += polygon_area(pts)
            except Exception:
                pass

        # 5. البلوكات (INSERT)
        elif dxftype == 'INSERT':
            block_name = entity.dxf.name
            if not block_name.startswith('*'):
                blocks_count[(block_name, layer)] += 1

    return {
        'lengths': lengths_by_layer,
        'areas': areas_by_layer,
        'blocks': blocks_count
    }

def print_summary(data):
    print("\n" + "="*65)
    print("                    ملخص حصر الكميات للمخطط                    ")
    print("="*65)

    print("\n--- [1] حصر الأطوال (Linear Quantities):")
    if data['lengths']:
        print(f"{'اسم الطبقة (Layer)':<35} | {'المجموع (م.ط / وحدة)':<15}")
        print("-" * 55)
        for layer, length in sorted(data['lengths'].items()):
            print(f"{layer:<35} | {length:<15.3f}")
    else:
        print("  لا توجد عناصر خطية.")

    print("\n--- [2] حصر المساحات (Area Quantities):")
    if data['areas']:
        print(f"{'اسم الطبقة (Layer)':<35} | {'المساحة (م² / وحدة²)':<15}")
        print("-" * 55)
        for layer, area in sorted(data['areas'].items()):
            print(f"{layer:<35} | {area:<15.3f}")
    else:
        print("  لا توجد أشكال مغلقة أو مساحات محسوبة.")

    print("\n--- [3] حصر الكتل والبلوكات (Block Counts):")
    if data['blocks']:
        print(f"{'اسم البلوك (Block Name)':<30} | {'الطبقة (Layer)':<20} | {'العدد':<8}")
        print("-" * 65)
        for (blk, layer), count in sorted(data['blocks'].items()):
            print(f"{blk:<30} | {layer:<20} | {count:<8}")
    else:
        print("  لا توجد بلوكات في المخطط.")
    print("="*65 + "\n")

def export_to_excel(data, output_file):
    """تصدير تقرير حصر الكميات والمقايسة التقديرية إلى ملف Excel احترافي متعدد الشيتات مطابق لنموذج المقايسة الهندسية"""
    try:
        import openpyxl
        from openpyxl.styles import Font, PatternFill, Alignment, Border, Side
        from openpyxl.utils import get_column_letter

        wb = openpyxl.Workbook()

        # Styles
        header_fill = PatternFill(start_color="107C41", end_color="107C41", fill_type="solid")
        header_font = Font(name="Segoe UI", size=10, bold=True, color="FFFFFF")
        total_fill = PatternFill(start_color="E8F5E9", end_color="E8F5E9", fill_type="solid")
        total_font = Font(name="Segoe UI", size=11, bold=True, color="107C41")
        center_align = Alignment(horizontal="center", vertical="center")
        right_align = Alignment(horizontal="right", vertical="center")
        thin_border = Border(
            left=Side(style='thin', color='D4D4D4'),
            right=Side(style='thin', color='D4D4D4'),
            top=Side(style='thin', color='D4D4D4'),
            bottom=Side(style='thin', color='D4D4D4')
        )
        total_border = Border(
            top=Side(style='thin', color='107C41'),
            bottom=Side(style='double', color='107C41')
        )

        # -------------------------------------------------------------
        # 1. Sheet 1: المقايسة_التقديرية_والتكاليف (YAKLAŞIK MALİYET)
        # -------------------------------------------------------------
        ws_cost = wb.active
        ws_cost.title = "المقايسة_التقديرية_والتكاليف"
        ws_cost.views.sheetView[0].rightToLeft = True

        headers_cost = [
            "Sıra No (م)",
            "İş Kalemi No (كود البند)",
            "İş Kaleminin Adı ve Açıklaması (بيان وتوصيف الأعمال)",
            "Ölçü Birimi (الوحدة)",
            "Miktar (الكمية)",
            "Birim Fiyat TL (سعر الوحدة)",
            "Tutar TL (الإجمالي)"
        ]
        ws_cost.append(headers_cost)
        for col_idx in range(1, len(headers_cost) + 1):
            c = ws_cost.cell(row=1, column=col_idx)
            c.fill = header_fill
            c.font = header_font
            c.alignment = center_align

        current_row = 2
        item_no = 1

        # الأطوال
        for layer, length in sorted(data['lengths'].items()):
            code = f"15.120.{1000 + item_no}"
            desc = f"أعمال مسارات وأطوال - طبقة {layer}"
            unit = "م.ط"
            qty = round(length, 3)
            price = 0
            formula = f"=E{current_row}*F{current_row}"
            ws_cost.append([item_no, code, desc, unit, qty, price, formula])
            for col_idx in range(1, 8):
                cell = ws_cost.cell(row=current_row, column=col_idx)
                cell.border = thin_border
                cell.alignment = right_align if col_idx == 3 else center_align
                if col_idx in (5, 6, 7):
                    cell.number_format = '#,##0.00'
            item_no += 1
            current_row += 1

        # المساحات
        for layer, area in sorted(data['areas'].items()):
            code = f"15.220.{1000 + item_no}"
            desc = f"أعمال مساحات وتشطيبات - طبقة {layer}"
            unit = "م²"
            qty = round(area, 3)
            price = 0
            formula = f"=E{current_row}*F{current_row}"
            ws_cost.append([item_no, code, desc, unit, qty, price, formula])
            for col_idx in range(1, 8):
                cell = ws_cost.cell(row=current_row, column=col_idx)
                cell.border = thin_border
                cell.alignment = right_align if col_idx == 3 else center_align
                if col_idx in (5, 6, 7):
                    cell.number_format = '#,##0.00'
            item_no += 1
            current_row += 1

        # البلوكات
        for (blk, layer), count in sorted(data['blocks'].items()):
            code = f"15.350.{1000 + item_no}"
            desc = f"توريد وتركيب {blk} [{layer}]"
            unit = "عدد"
            qty = count
            price = 0
            formula = f"=E{current_row}*F{current_row}"
            ws_cost.append([item_no, code, desc, unit, qty, price, formula])
            for col_idx in range(1, 8):
                cell = ws_cost.cell(row=current_row, column=col_idx)
                cell.border = thin_border
                cell.alignment = right_align if col_idx == 3 else center_align
                if col_idx in (5, 6, 7):
                    cell.number_format = '#,##0.00' if col_idx != 5 else '#,##0'
            item_no += 1
            current_row += 1

        # صف الإجمالي العام
        if current_row > 2:
            ws_cost.append(["المجموع الكلي التقديري للمشروع (TOPLAM TUTAR):", None, None, None, None, None, f"=SUM(G2:G{current_row - 1})"])
            ws_cost.merge_cells(start_row=current_row, start_column=1, end_row=current_row, end_column=6)
            t_cell = ws_cost.cell(row=current_row, column=1)
            t_cell.fill = total_fill
            t_cell.font = total_font
            t_cell.alignment = center_align
            s_cell = ws_cost.cell(row=current_row, column=7)
            s_cell.fill = total_fill
            s_cell.font = total_font
            s_cell.alignment = center_align
            s_cell.border = total_border
            s_cell.number_format = '#,##0.00'

        col_widths = [12, 22, 45, 15, 18, 20, 22]
        for idx, w in enumerate(col_widths, 1):
            ws_cost.column_dimensions[get_column_letter(idx)].width = w

        # -------------------------------------------------------------
        # 2. Sheet 2: دفتر_حصر_الأطوال_والمساحات (METRAJ CETVELİ)
        # -------------------------------------------------------------
        ws_metraj = wb.create_sheet(title="دفتر_حصر_الأطوال_والمساحات")
        ws_metraj.views.sheetView[0].rightToLeft = True

        headers_m = [
            "NO (م)",
            "İşin Açıklanması / Layer (بيان العنصر / الطبقة)",
            "Kategori (نوع الحصر)",
            "Benzer (المتشابه)",
            "Boy / Alan (المقاس)",
            "MİNHA (الخصم)",
            "TOPLAM (الإجمالي)",
            "Birim (الوحدة)"
        ]
        ws_metraj.append(headers_m)
        for col_idx in range(1, len(headers_m) + 1):
            c = ws_metraj.cell(row=1, column=col_idx)
            c.fill = header_fill
            c.font = header_font
            c.alignment = center_align

        m_row = 2
        m_idx = 1
        for layer, length in sorted(data['lengths'].items()):
            formula = f"=(D{m_row}*E{m_row})-F{m_row}"
            ws_metraj.append([m_idx, layer, "أطوال (Linear)", 1, round(length, 3), 0, formula, "م.ط"])
            for c_idx in range(1, 9):
                cell = ws_metraj.cell(row=m_row, column=c_idx)
                cell.border = thin_border
                cell.alignment = right_align if c_idx == 2 else center_align
                if c_idx in (5, 6, 7):
                    cell.number_format = '#,##0.00'
            m_row += 1
            m_idx += 1

        for layer, area in sorted(data['areas'].items()):
            formula = f"=(D{m_row}*E{m_row})-F{m_row}"
            ws_metraj.append([m_idx, layer, "مساحات (Area)", 1, round(area, 3), 0, formula, "م²"])
            for c_idx in range(1, 9):
                cell = ws_metraj.cell(row=m_row, column=c_idx)
                cell.border = thin_border
                cell.alignment = right_align if c_idx == 2 else center_align
                if c_idx in (5, 6, 7):
                    cell.number_format = '#,##0.00'
            m_row += 1
            m_idx += 1

        m_widths = [10, 35, 18, 12, 16, 12, 16, 12]
        for idx, w in enumerate(m_widths, 1):
            ws_metraj.column_dimensions[get_column_letter(idx)].width = w

        # -------------------------------------------------------------
        # 3. Sheet 3: حصر_الأبواب_والشبابيك_والبلوكات (KAPI, PENCERE VE BLOKLAR)
        # -------------------------------------------------------------
        ws_blocks = wb.create_sheet(title="حصر_الأبواب_والشبابيك")
        ws_blocks.views.sheetView[0].rightToLeft = True

        headers_b = [
            "NO (م)",
            "İşin Açıklanması / Blok (اسم البلوك / الرمز)",
            "Layer (الطبقة)",
            "Benzer (المتشابه)",
            "Adet (العدد المحصور)",
            "TOPLAM (الإجمالي)",
            "Birim (الوحدة)"
        ]
        ws_blocks.append(headers_b)
        for col_idx in range(1, len(headers_b) + 1):
            c = ws_blocks.cell(row=1, column=col_idx)
            c.fill = header_fill
            c.font = header_font
            c.alignment = center_align

        b_row = 2
        b_idx = 1
        for (blk, layer), count in sorted(data['blocks'].items()):
            formula = f"=D{b_row}*E{b_row}"
            ws_blocks.append([b_idx, blk, layer, 1, count, formula, "عدد (adet)"])
            for c_idx in range(1, 8):
                cell = ws_blocks.cell(row=b_row, column=c_idx)
                cell.border = thin_border
                cell.alignment = right_align if c_idx == 2 else center_align
                if c_idx in (4, 5, 6):
                    cell.number_format = '#,##0'
            b_row += 1
            b_idx += 1

        if b_row > 2:
            ws_blocks.append(["إجمالي عدد البلوكات (TOPLAM ADET):", None, None, None, None, f"=SUM(F2:F{b_row - 1})", "عدد"])
            ws_blocks.merge_cells(start_row=b_row, start_column=1, end_row=b_row, end_column=5)
            t_cell = ws_blocks.cell(row=b_row, column=1)
            t_cell.fill = total_fill
            t_cell.font = total_font
            t_cell.alignment = center_align
            s_cell = ws_blocks.cell(row=b_row, column=6)
            s_cell.fill = total_fill
            s_cell.font = total_font
            s_cell.alignment = center_align
            s_cell.border = total_border
            s_cell.number_format = '#,##0'
            u_cell = ws_blocks.cell(row=b_row, column=7)
            u_cell.fill = total_fill
            u_cell.font = total_font
            u_cell.alignment = center_align

        b_widths = [10, 30, 22, 14, 16, 16, 14]
        for idx, w in enumerate(b_widths, 1):
            ws_blocks.column_dimensions[get_column_letter(idx)].width = w

        wb.save(output_file)
        print(f"[✓] تم حفظ وتصدير ملف Excel متعدد الشيتات بنجاح في: {output_file}")
        return True

    except ImportError:
        print("[!] مكتبة openpyxl غير متوفرة، جاري التصدير بصيغة CSV كبديل...")
        csv_file = os.path.splitext(output_file)[0] + ".csv"
        export_to_csv(data, csv_file)
        return False

def export_to_csv(data, output_csv):
    with open(output_csv, mode='w', encoding='utf-8-sig', newline='') as f:
        writer = csv.writer(f)
        writer.writerow(["تقرير حصر الكميات - DXF Quantity Takeoff Report"])
        writer.writerow([])
        
        # الأطوال
        writer.writerow(["--- [1] حصر الأطوال (Linear Takeoff) ---"])
        writer.writerow(["نوع الحصر", "اسم الطبقة (Layer)", "الكمية الإجمالية", "الوحدة"])
        for layer, length in sorted(data['lengths'].items()):
            writer.writerow(["أطوال", layer, f"{length:.3f}", "متر طولي / وحدة"])
        writer.writerow([])

        # المساحات
        writer.writerow(["--- [2] حصر المساحات (Area Takeoff) ---"])
        writer.writerow(["نوع الحصر", "اسم الطبقة (Layer)", "المساحة الإجمالية", "الوحدة"])
        for layer, area in sorted(data['areas'].items()):
            writer.writerow(["مساحات", layer, f"{area:.3f}", "متر مربع / وحدة²"])
        writer.writerow([])

        # البلوكات
        writer.writerow(["--- [3] حصر البلوكات والعناصر (Block Counts) ---"])
        writer.writerow(["نوع الحصر", "اسم البلوك (Block Name)", "الطبقة (Layer)", "العدد", "الوحدة"])
        for (blk, layer), count in sorted(data['blocks'].items()):
            writer.writerow(["بلوكات", blk, layer, count, "عدد"])

    print(f"[✓] تم حفظ التقرير بنجاح في: {output_csv}")

if __name__ == '__main__':
    if len(sys.argv) < 2:
        print("طريقة الاستخدام:")
        print("  python qto_dxf.py <drawing.dxf> [output.xlsx]")
        sys.exit(0)

    input_file = sys.argv[1]
    base, _ = os.path.splitext(input_file)
    output_file = sys.argv[2] if len(sys.argv) > 2 else f"{base}_حصر_الكميات.xlsx"

    results = process_dxf(input_file)
    if results:
        print_summary(results)
        if output_file.endswith('.csv'):
            export_to_csv(results, output_file)
        else:
            export_to_excel(results, output_file)
