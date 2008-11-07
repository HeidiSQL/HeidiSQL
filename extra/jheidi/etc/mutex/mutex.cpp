#include <windows.h>

int WINAPI WinMain(HINSTANCE hInst, HINSTANCE hPrev, LPSTR lpCmdLine, int nShowCmd)
{
	

	//looks redundant but we actually have to get the message loop running asap
	//to avoid a long hourglass being shown.
	MSG msg;
	
	while (PeekMessage(&msg, NULL, WM_PAINT, WM_PAINT, PM_REMOVE))
			DispatchMessage(&msg);

	HANDLE h = GetCurrentProcess();
    
    while (WaitForSingleObject(h,100) == WAIT_TIMEOUT)
    {
		while (PeekMessage(&msg, NULL, WM_PAINT, WM_PAINT, PM_REMOVE))
			DispatchMessage(&msg);
	}
    CloseHandle( h );

		

}