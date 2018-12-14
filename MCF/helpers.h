#pragma once

#include "common.h"

namespace MCF {

enum class ConsoleColor
{
	Red,
	DarkRed,
	Blue,
	DarkBlue,
	Green,
	DarkGreen,
	Cyan,
	Yellow,
	DarkYellow,
	Magenta,
	White,
	Grey,
};

MCF_API void SetConsoleColor(const ConsoleColor& color = ConsoleColor::Grey);
MCF_API void ResetConsoleColor();
MCF_API void ClearConsole(char fill = ' ');
MCF_API void SetCursorVisibility(bool visible = true);
MCF_API int GetConsoleWidth();
MCF_API int GetCursorTop();
MCF_API void SetCursorPosition(int x, int y);

enum class KeyInputKind
{
	Control,
	Enter,
	Escape,
	LeftArrow,
	RightArrow,
	UpArrow,
	DownArrow,
	WaitForArrow,
	Backspace,
	Delete,
	Home,
	End,
	Tab,
	PageUp,
	PageDown,

	General,
};

MCF_API int ReadKeyFromConsole();
MCF_API KeyInputKind DecideKeyInputKind(const int input);

/// string helpers
MCF_API bool IsStringBlank(const std::string& s);
MCF_API bool StringStartsWith(const string& sample, const string& beginning);
MCF_API bool StringEndsWith(const string& sample, const string& ending);
MCF_API string TrimString(const string& text);
MCF_API string TrimStringStart(const string& text);
MCF_API string TrimStringEnd(const string& text);
MCF_API string StringJoin(const vector<string>& strs, const char seperator = ' ');
MCF_API vector<string> StringSplit(const string& s, const char delimiter = ' ');

}//MCF