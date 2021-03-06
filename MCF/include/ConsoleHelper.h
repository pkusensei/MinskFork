#pragma once

#include "common.h"

namespace MCF {

enum class ConsoleColor
{
	Black,
	Red,
	Green,
	Yellow,
	Blue,
	Magenta,
	Cyan,
	White,
	DarkRed,
	DarkBlue,
	DarkGreen,
	DarkYellow,

	Gray,
	DarkGray,
};

MCF_API bool EnableVTMode();
MCF_API void SetConsoleColor(ConsoleColor color = ConsoleColor::Gray);
MCF_API void ResetConsoleColor();
MCF_API void ClearConsole();
MCF_API void SetCursorVisibility(bool visible = true);
MCF_API int GetConsoleHeight();
MCF_API int GetConsoleWidth();
MCF_API size_t GetCursorTop();
MCF_API void SetCursorPosition(size_t x, size_t y);

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

struct KeyInfo
{
	// PascalCase as interface
	const char Key;
	const bool IsFunctionalKey;
	const KeyInputKind Kind;

	KeyInfo(char key, bool isFuncKey, KeyInputKind kind)noexcept
		:Key(key), IsFunctionalKey(isFuncKey), Kind(kind)
	{
	}
};

MCF_API KeyInfo ReadKeyFromConsole();
MCF_API KeyInputKind DecideKeyInputKind(int input)noexcept;

}//MCF
