#include "ConsoleHelper.h"

#include <iostream>
#include <conio.h>

#ifdef _WIN32
#include "stdafx.h"
#endif // _WIN32

namespace MCF {

namespace {

constexpr auto CSI = "\x1b[";
constexpr auto BRIGHT = "1;";
constexpr auto DEFAULT = "0;";

constexpr auto BLACK = "30m";
constexpr auto RED = "31m";
constexpr auto GREEN = "32m";
constexpr auto YELLOW = "33m";
constexpr auto BLUE = "34m";
constexpr auto MAGENTA = "35m";
constexpr auto CYAN = "36m";
constexpr auto WHITE = "37m";

} //namespace

bool EnableVTMode()
{
	// Set output mode to handle virtual terminal sequences
	HANDLE hOut = GetStdHandle(STD_OUTPUT_HANDLE);
	if (hOut == INVALID_HANDLE_VALUE) return false;


	DWORD dwMode = 0;
	if (!GetConsoleMode(hOut, &dwMode)) return false;


	dwMode |= ENABLE_VIRTUAL_TERMINAL_PROCESSING;
	if (!SetConsoleMode(hOut, dwMode)) return false;

	return true;
}

void SetConsoleColor(ConsoleColor color)
{
	std::cout << CSI;
	switch (color)
	{
		case ConsoleColor::DarkGray:
			std::cout << BRIGHT << BLACK;
			break;
		case ConsoleColor::Red:
			std::cout << BRIGHT << RED;
			break;
		case ConsoleColor::Green:
			std::cout << BRIGHT << GREEN;
			break;
		case ConsoleColor::Yellow:
			std::cout << BRIGHT << YELLOW;
			break;
		case ConsoleColor::Blue:
			std::cout << BRIGHT << BLUE;
			break;
		case ConsoleColor::Magenta:
			std::cout << BRIGHT << MAGENTA;
			break;
		case ConsoleColor::Cyan:
			std::cout << BRIGHT << CYAN;
			break;
		case ConsoleColor::White:
			std::cout << BRIGHT << WHITE;
			break;
		case ConsoleColor::DarkRed:
			std::cout << DEFAULT << RED;
			break;
		case ConsoleColor::DarkGreen:
			std::cout << DEFAULT << GREEN;
			break;
		case ConsoleColor::DarkYellow:
			std::cout << DEFAULT << YELLOW;
			break;
		case ConsoleColor::DarkBlue:
			std::cout << DEFAULT << BLUE;
			break;
		case ConsoleColor::Gray:
		default:
			std::cout << DEFAULT << WHITE;
			break;
	}
}

void ResetConsoleColor()
{
	SetConsoleColor();
}

void ClearConsole()
{
	COORD topLeft = { 0, 0 };
	HANDLE console = GetStdHandle(STD_OUTPUT_HANDLE);
	CONSOLE_SCREEN_BUFFER_INFO screen;
	DWORD written;

	GetConsoleScreenBufferInfo(console, &screen);
	FillConsoleOutputCharacterA(
		console, ' ', screen.dwSize.X * screen.dwSize.Y, topLeft, &written
	);
	FillConsoleOutputAttribute(
		console, FOREGROUND_GREEN | FOREGROUND_RED | FOREGROUND_BLUE,
		screen.dwSize.X * screen.dwSize.Y, topLeft, &written
	);
	SetConsoleCursorPosition(console, topLeft);
}

void SetCursorVisibility(bool visible)
{
	std::cout << CSI;
	std::cout << (visible ? "?25h" : "?25l");

	//HANDLE hStdout = GetStdHandle(STD_OUTPUT_HANDLE);
	//CONSOLE_CURSOR_INFO info;
	//info.dwSize = 100;
	//info.bVisible = visible ? TRUE : FALSE;
	//SetConsoleCursorInfo(hStdout, &info);
}

int GetConsoleHeight()
{
	CONSOLE_SCREEN_BUFFER_INFO csbiInfo;
	GetConsoleScreenBufferInfo(GetStdHandle(STD_OUTPUT_HANDLE), &csbiInfo);
	return static_cast<int>(csbiInfo.dwSize.Y);
}

int GetConsoleWidth()
{
	CONSOLE_SCREEN_BUFFER_INFO csbiInfo;
	GetConsoleScreenBufferInfo(GetStdHandle(STD_OUTPUT_HANDLE), &csbiInfo);
	return static_cast<int>(csbiInfo.dwSize.X);
}

size_t GetCursorTop()
{
	auto hStdout = GetStdHandle(STD_OUTPUT_HANDLE);
	CONSOLE_SCREEN_BUFFER_INFO csbiInfo;
	GetConsoleScreenBufferInfo(hStdout, &csbiInfo);
	return static_cast<size_t>(csbiInfo.dwCursorPosition.Y);
}

void SetCursorPosition(size_t x, size_t y)
{
	//HACK where should this +1 be
	std::cout << CSI << y << ';' << x + 1 << 'f';
}

KeyInfo ReadKeyFromConsole()
{
	auto k = _getch();
	auto isFunctionalKey = false;
	if (k == 0 || k == 0xE0)
	{
		k = _getch();
		isFunctionalKey = true;
		// HACK remaps the return value from the second call to _getch() to VK codes.
		switch (k)
		{
			case 71: k = VK_HOME; break;
			case 79: k = VK_END; break;
			case 83: k = VK_DELETE; break;
			case 73: k = VK_PRIOR; break;
			case 81: k = VK_NEXT; break;
			case 72: k = VK_UP; break;
			case 80: k = VK_DOWN; break;
			case 75: k = VK_LEFT; break;
			case 77: k = VK_RIGHT; break;
		}
	}
	auto kind = DecideKeyInputKind(k);
	// HACK 
	switch (kind)
	{
		case KeyInputKind::Control:
		case KeyInputKind::Enter:
		case KeyInputKind::Escape:
		case KeyInputKind::Backspace:
		case KeyInputKind::Tab:
			isFunctionalKey = true;
		default:
			break;
	}
	return KeyInfo(static_cast<char>(k), isFunctionalKey, kind);
}

KeyInputKind DecideKeyInputKind(int input)noexcept
{
	switch (input)
	{
		case VK_CONTROL: case VK_LCONTROL: case VK_RCONTROL:
			return KeyInputKind::Control;
		case VK_RETURN:return KeyInputKind::Enter;
		case VK_ESCAPE:return KeyInputKind::Escape;
		case VK_BACK:return KeyInputKind::Backspace;
		case VK_TAB:return KeyInputKind::Tab;

		case VK_LEFT:return KeyInputKind::LeftArrow;
		case VK_RIGHT:return KeyInputKind::RightArrow;
		case VK_UP:return KeyInputKind::UpArrow;
		case VK_DOWN:return KeyInputKind::DownArrow;

		case VK_HOME:return KeyInputKind::Home;
		case VK_END:return KeyInputKind::End;
		case VK_DELETE:return KeyInputKind::Delete;
		case VK_PRIOR:return KeyInputKind::PageUp;
		case VK_NEXT:return KeyInputKind::PageDown;
		default:
			return KeyInputKind::General;
	}
}

}//MCF
