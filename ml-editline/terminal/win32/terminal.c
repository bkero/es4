/*
 * Copyright (c) 2007 David Herman
 * 
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 * 
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */

#include <windows.h>
#include <stdio.h>
#include <stdlib.h>

#if defined(TERMINAL_EXPORTS)
#define TERMINAL_API __declspec(dllexport)
#else
#define TERMINAL_API __declspec(dllimport)
#endif

BOOL APIENTRY DllMain(HMODULE hModule, DWORD reason, LPVOID reserved)
{
    return TRUE;
}

TERMINAL_API void GetWindowSize(UINT32 *rows, UINT32 *cols)
{
    CONSOLE_SCREEN_BUFFER_INFO info;
    HANDLE hOut;

    hOut = GetStdHandle(STD_OUTPUT_HANDLE);
    GetConsoleScreenBufferInfo(hOut, &info);

    if (cols) *cols = info.dwSize.X;
    if (rows) *rows = info.dwSize.Y;
}

TERMINAL_API void GetPosition(UINT32 *row, UINT32 *col)
{
    CONSOLE_SCREEN_BUFFER_INFO info;

    GetConsoleScreenBufferInfo(GetStdHandle(STD_OUTPUT_HANDLE), &info);

    if (row) *row = info.dwCursorPosition.Y;
    if (col) *col = info.dwCursorPosition.X;
}

TERMINAL_API void SetPosition(UINT32 row, UINT32 col)
{
    COORD posn;
    HANDLE hOut;

    hOut = GetStdHandle(STD_OUTPUT_HANDLE);
    posn.X = col;
    posn.Y = row;
    SetConsoleCursorPosition(hOut, posn);
}

TERMINAL_API void FillOutput(UINT32 row, UINT32 col, UINT32 length, char ch)
{
    DWORD written;
    COORD coord;

    coord.X = col;
    coord.Y = row;
    FillConsoleOutputCharacter(GetStdHandle(STD_OUTPUT_HANDLE), ch, length, coord, &written);
}

TERMINAL_API void ReadKey(char *ch, UINT32 *special, UINT32 *alt)
{
    HANDLE hIn;
    INPUT_RECORD irBuffer;
    int count;

    hIn = GetStdHandle(STD_INPUT_HANDLE);
    SetConsoleMode(hIn, ENABLE_PROCESSED_INPUT);
    *alt = 0;

    while (TRUE)
    {
        PeekConsoleInput(hIn, &irBuffer, 1, &count);
        if (count != 0)
        {
            if ((irBuffer.EventType == KEY_EVENT) && irBuffer.Event.KeyEvent.bKeyDown)
            {
                *alt = ((irBuffer.Event.KeyEvent.dwControlKeyState & (LEFT_ALT_PRESSED | RIGHT_ALT_PRESSED)) != 0);
                if (irBuffer.Event.KeyEvent.uChar.AsciiChar == '\0')
                {
                    ReadConsoleInput(hIn, &irBuffer, 1, &count);
                    *special = irBuffer.Event.KeyEvent.wVirtualKeyCode;
                    *ch = 0;
                }
                else
                {
                    ReadConsole(hIn, ch, 1, &count, NULL);
                    *special = 0;
                }
                return;
            }
            else ReadConsoleInput(hIn, &irBuffer, 1, &count);
        }
        else WaitForSingleObject(hIn, INFINITE);
    }
}
