#include "stdafx.h"
#include "helpers.h"

#include <algorithm>
#include <cctype>
#include <sstream>

#include <conio.h>

namespace MCF {

void SetConsoleColor(const ConsoleColor& color)
{
	HANDLE hStdout = GetStdHandle(STD_OUTPUT_HANDLE);
	//CONSOLE_SCREEN_BUFFER_INFO csbiInfo;
	//GetConsoleScreenBufferInfo(hStdout, &csbiInfo);
	//WORD wOldColorAttrs = csbiInfo.wAttributes;
	switch (color)
	{
		case ConsoleColor::Red:
			SetConsoleTextAttribute(hStdout, FOREGROUND_RED | FOREGROUND_INTENSITY);
			break;
		case ConsoleColor::DarkRed:
			SetConsoleTextAttribute(hStdout, FOREGROUND_RED);
			break;
		case ConsoleColor::Blue:
			SetConsoleTextAttribute(hStdout, FOREGROUND_BLUE | FOREGROUND_INTENSITY);
			break;
		case ConsoleColor::DarkBlue:
			SetConsoleTextAttribute(hStdout, FOREGROUND_BLUE);
			break;
		case ConsoleColor::Green:
			SetConsoleTextAttribute(hStdout, FOREGROUND_GREEN | FOREGROUND_INTENSITY);
			break;
		case ConsoleColor::DarkGreen:
			SetConsoleTextAttribute(hStdout, FOREGROUND_GREEN);
			break;
		case ConsoleColor::Cyan:
			SetConsoleTextAttribute(hStdout, FOREGROUND_GREEN | FOREGROUND_BLUE | FOREGROUND_INTENSITY);
			break;
		case ConsoleColor::Yellow:
			SetConsoleTextAttribute(hStdout, FOREGROUND_GREEN | FOREGROUND_RED | FOREGROUND_INTENSITY);
			break;
		case ConsoleColor::DarkYellow:
			SetConsoleTextAttribute(hStdout, FOREGROUND_GREEN | FOREGROUND_RED);
			break;
		case ConsoleColor::Magenta:
			SetConsoleTextAttribute(hStdout, FOREGROUND_RED | FOREGROUND_BLUE);
			break;
		case ConsoleColor::White:
			SetConsoleTextAttribute(hStdout, FOREGROUND_RED | FOREGROUND_BLUE | FOREGROUND_GREEN | FOREGROUND_INTENSITY);
			break;
		case ConsoleColor::Grey:
		default:
			SetConsoleTextAttribute(hStdout, FOREGROUND_RED | FOREGROUND_BLUE | FOREGROUND_GREEN);
			break;
	}
}

void ResetConsoleColor()
{
	SetConsoleColor();
}

void ClearConsole(char fill)
{
	COORD t1 = {0, 0};
	HANDLE hStdout = GetStdHandle(STD_OUTPUT_HANDLE);
	CONSOLE_SCREEN_BUFFER_INFO csbiInfo;
	GetConsoleScreenBufferInfo(hStdout, &csbiInfo);
	DWORD written;
	DWORD cells = csbiInfo.dwSize.X*csbiInfo.dwSize.Y;
	FillConsoleOutputCharacter(hStdout, fill, cells, t1, &written);
	FillConsoleOutputAttribute(hStdout, csbiInfo.wAttributes, cells, t1, &written);
	SetConsoleCursorPosition(hStdout, t1);
}

void SetCursorVisibility(bool visible)
{
	HANDLE hStdout = GetStdHandle(STD_OUTPUT_HANDLE);
	CONSOLE_CURSOR_INFO info;
	info.dwSize = 100;
	info.bVisible = visible ? TRUE : FALSE;
	SetConsoleCursorInfo(hStdout, &info);
}

int GetConsoleWidth()
{
	CONSOLE_SCREEN_BUFFER_INFO csbiInfo;
	GetConsoleScreenBufferInfo(GetStdHandle(STD_OUTPUT_HANDLE), &csbiInfo);
	return static_cast<int>(csbiInfo.dwSize.X);
}

int GetCursorTop()
{
	auto hStdout = GetStdHandle(STD_OUTPUT_HANDLE);
	CONSOLE_SCREEN_BUFFER_INFO csbiInfo;
	GetConsoleScreenBufferInfo(hStdout, &csbiInfo);
	return static_cast<int>(csbiInfo.dwCursorPosition.Y);
}

void SetCursorPosition(int x, int y)
{
	auto hStdout = GetStdHandle(STD_OUTPUT_HANDLE);
	COORD c;
	c.X = x;
	c.Y = y;
	SetConsoleCursorPosition(hStdout, c);
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
	}
	return {static_cast<char>(k), isFunctionalKey, kind};
}

KeyInputKind DecideKeyInputKind(const int input)
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

bool IsStringBlank(const std::string& s)
{
	return std::all_of(s.begin(), s.end(), std::isspace);
}

bool StringStartsWith(const string & sample, const string & beginning)
{
	if (sample.length() < beginning.length())return false;
	return std::equal(beginning.begin(), beginning.end(), sample.begin());
}

bool StringEndsWith(const string & sample, const string & ending)
{
	if (sample.length() < ending.length()) return false;
	return std::equal(ending.rbegin(), ending.rend(), sample.rbegin());
}

string TrimString(const string & text)
{
	return TrimStringStart(TrimStringEnd(text));
}

string TrimStringStart(const string & text)
{
	auto result = text;
	result.erase(result.begin(), std::find_if(result.begin(), result.end(),
											  [](char ch) {return !std::isspace(ch); }));
	return result;
}

string TrimStringEnd(const string & text)
{
	auto result = text;
	result.erase(std::find_if(result.rbegin(), result.rend(),
							  [](char ch) {return !std::isspace(ch); }).base(), result.end());
	return result;
}

string StringJoin(const vector<string>& strs, const char seperator)
{
	auto result = string();
	for (const auto& it : strs)
	{
		result += it + seperator;
	}
	if (!result.empty())
		result.erase(result.length() - 1);
	return result;
}

vector<string> StringSplit(const string & s, const char delimiter)
{
	auto result = vector<string>();
	auto token = string();
	auto isstream = std::istringstream(s);
	while (std::getline(isstream, token, delimiter))
	{
		result.emplace_back(token);
	}
	return result;
}

}//MCF