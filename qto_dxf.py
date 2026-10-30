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

def distance_2d(p1, p2):
    return math.sqrt((p2[0] - p1[0])**2 + (p2[1] - p1[1])**2)

def polygon_area(points):
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

def process_dxf(filepath):
    try:
        import ezdxf
    except ImportError:
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
        print("  python qto_dxf.py <drawing.dxf> [output.csv]")
        sys.exit(0)

    input_file = sys.argv[1]
    base, _ = os.path.splitext(input_file)
    output_file = sys.argv[2] if len(sys.argv) > 2 else f"{base}_حصر_الكميات.csv"

    results = process_dxf(input_file)
    if results:
        print_summary(results)
        export_to_csv(results, output_file)
