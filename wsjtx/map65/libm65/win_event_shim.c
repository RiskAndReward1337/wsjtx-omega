// win_event_shim.c
#include <windows.h>

int win_set_event(void *hEvent)
{
    return SetEvent((HANDLE)hEvent);
}
