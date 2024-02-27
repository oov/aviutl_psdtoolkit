#pragma once

#include "point.h"

#include <math.h>

#define WIN32_LEAN_AND_MEAN
#include <windows.h>

static inline void save_bitmap_to_file(BITMAPINFO const *const bi, void const *const image, wchar_t const *const path) {
  BITMAPFILEHEADER bfh;
  DWORD written = 0;
  HANDLE h;

  bfh.bfType = 0x4D42;
  bfh.bfSize = sizeof(BITMAPFILEHEADER) + sizeof(BITMAPINFOHEADER) + bi->bmiHeader.biSizeImage;
  bfh.bfReserved1 = 0;
  bfh.bfReserved2 = 0;
  bfh.bfOffBits = sizeof(BITMAPFILEHEADER) + sizeof(BITMAPINFOHEADER);

  h = CreateFileW(path, GENERIC_WRITE, 0, NULL, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, NULL);
  if (h == INVALID_HANDLE_VALUE) {
    return;
  }

  WriteFile(h, &bfh, sizeof(BITMAPFILEHEADER), &written, NULL);
  WriteFile(h, bi, sizeof(BITMAPINFOHEADER), &written, NULL);
  WriteFile(h, image, bi->bmiHeader.biSizeImage, &written, NULL);

  CloseHandle(h);
}

static inline void draw_arrow(HDC hdc, int ax, int ay, int bx, int by) {
  double const angle = atan2(by - ay, bx - ax);
  double const length = 10.;
  double const pi = 3.14159265358979323846;
  double const angle1 = angle + pi / 6.;
  double const angle2 = angle - pi / 6.;
  int const x1 = (int)(bx - length * cos(angle1));
  int const y1 = (int)(by - length * sin(angle1));
  int const x2 = (int)(bx - length * cos(angle2));
  int const y2 = (int)(by - length * sin(angle2));
  MoveToEx(hdc, ax, ay, NULL);
  LineTo(hdc, bx, by);
  MoveToEx(hdc, bx, by, NULL);
  LineTo(hdc, x1, y1);
  MoveToEx(hdc, bx, by, NULL);
  LineTo(hdc, x2, y2);
}

static inline void create_bitmap(struct point const *const l,
                                 size_t const l_len,
                                 struct point const *const r,
                                 size_t const r_len,
                                 struct point const *const arrows,
                                 size_t const arrows_len,
                                 double const orientation,
                                 size_t const width,
                                 size_t const height,
                                 wchar_t const *const path) {
  void *image;
  HDC ddc, dc;
  HBITMAP bmp;

  BITMAPINFO bi = {.bmiHeader = {
                       .biSize = sizeof(BITMAPINFOHEADER),
                       .biWidth = (LONG)width,
                       .biHeight = (LONG)height,
                       .biPlanes = 1,
                       .biBitCount = 32,
                       .biCompression = BI_RGB,
                       .biSizeImage = (DWORD)(width * 4 * height),
                   }};

  ddc = GetDC(NULL);
  dc = CreateCompatibleDC(ddc);
  ReleaseDC(NULL, ddc);

  bmp = CreateDIBSection(dc, &bi, DIB_RGB_COLORS, &image, NULL, 0);
  SelectObject(dc, bmp);

  HPEN pen = CreatePen(PS_SOLID, 1, RGB(255, 255, 0));
  HPEN oldpen = SelectObject(dc, pen);
  if (l_len) {
    BeginPath(dc);
    MoveToEx(dc, (int)l[0].x, (int)l[0].y, NULL);
    for (size_t i = 1; i < l_len; i++) {
      LineTo(dc, (int)l[i].x, (int)l[i].y);
    }
    CloseFigure(dc);
    EndPath(dc);
    StrokePath(dc);
  }
  SelectObject(dc, oldpen);
  DeleteObject(pen);

  pen = CreatePen(PS_SOLID, 1, RGB(0, 255, 255));
  oldpen = SelectObject(dc, pen);
  if (r_len) {
    BeginPath(dc);
    MoveToEx(dc, (int)r[0].x, (int)r[0].y, NULL);
    for (size_t i = 1; i < r_len; i++) {
      LineTo(dc, (int)r[i].x, (int)r[i].y);
    }
    CloseFigure(dc);
    EndPath(dc);
    StrokePath(dc);
  }
  SelectObject(dc, oldpen);
  DeleteObject(pen);

  oldpen = SelectObject(dc, GetStockObject(WHITE_PEN));
  BeginPath(dc);
  if (orientation > 0) {
    for (size_t i = 0; i < arrows_len - 1; ++i) {
      draw_arrow(dc, (int)arrows[i].x, (int)arrows[i].y, (int)arrows[i + 1].x, (int)arrows[i + 1].y);
    }
  } else if (orientation < 0) {
    for (size_t i = arrows_len - 1; i > 0; --i) {
      draw_arrow(dc, (int)arrows[i].x, (int)arrows[i].y, (int)arrows[i - 1].x, (int)arrows[i - 1].y);
    }
  } else {
    MoveToEx(dc, (int)arrows[0].x, (int)arrows[0].y, NULL);
    for (size_t i = 1; i < arrows_len; ++i) {
      LineTo(dc, (int)arrows[i].x, (int)arrows[i].y);
    }
  }
  EndPath(dc);
  StrokePath(dc);
  SelectObject(dc, oldpen);

  for (size_t i = 0; i < l_len; i++) {
    FillRect(dc,
             &(RECT){(int)l[i].x - 2, (int)l[i].y - 2, (int)l[i].x + 2, (int)l[i].y + 2},
             (HBRUSH)GetStockObject(GRAY_BRUSH));
  }
  for (size_t i = 0; i < r_len; i++) {
    FillRect(dc,
             &(RECT){(int)r[i].x - 2, (int)r[i].y - 2, (int)r[i].x + 2, (int)r[i].y + 2},
             (HBRUSH)GetStockObject(GRAY_BRUSH));
  }

  if (arrows_len > 0) {
    FillRect(dc,
             &(RECT){(int)arrows[0].x - 4, (int)arrows[0].y - 4, (int)arrows[0].x + 4, (int)arrows[0].y + 4},
             (HBRUSH)GetStockObject(WHITE_BRUSH));
  }

  save_bitmap_to_file(&bi, image, path);

  DeleteObject(bmp);
  ReleaseDC(NULL, dc);
}
