#include <windows.h>

int WINAPI WinMain(HINSTANCE hInst, HINSTANCE hPrev, LPSTR lpCmdLine, int nShowCmd)
{
   MSG Msg;
   while(GetMessage(&Msg, NULL, 0, 0) > 0)
   {
    TranslateMessage(&Msg);
    DispatchMessage(&Msg);
   }
}
