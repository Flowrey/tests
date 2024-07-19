#ifndef WIN32_LEAN_AND_MEAN
#define WIN32_LEAN_AND_MEAN
#endif

#include <iostream>

#include <windows.h>
#include <mmdeviceapi.h>
#include <endpointvolume.h>

#include <winsock2.h>
#include <ws2tcpip.h>
#include <stdio.h>

#pragma comment(lib, "Ws2_32.lib")

#define DEFAULT_PORT "27015"

const CLSID CLSID_MMDeviceEnumerator = __uuidof(MMDeviceEnumerator);
const IID IID_IMMDeviceEnumerator = __uuidof(IMMDeviceEnumerator);
const IID IID_IAudioEndpointVolume = __uuidof(IAudioEndpointVolume);

int main() 
{
    // Initialize Winsock
    WSADATA wsaData;
    int iResult = WSAStartup(MAKEWORD(2, 2), &wsaData);
    if (iResult != 0) {
        printf("WSAStartup failed: %d\n", iResult);
        return 1;
    }

    struct addrinfo* result = NULL, * ptr = NULL, hints;
    ZeroMemory(&hints, sizeof(hints));
    hints.ai_family = AF_INET;
    hints.ai_socktype = SOCK_DGRAM;
    hints.ai_protocol = IPPROTO_UDP;
    hints.ai_flags = AI_PASSIVE;

    // Resolve the local address and port to be used by the server
    iResult = getaddrinfo(NULL, DEFAULT_PORT, &hints, &result);
    if (iResult != 0) {
        printf("getaddrinfo failed: %d\n", iResult);
        WSACleanup();
        return 1;
    }

    SOCKET ListenSocket = socket(result->ai_family, result->ai_socktype, result->ai_protocol);
    if (ListenSocket == INVALID_SOCKET) {
        std::cout << "socket function failed with error = " << WSAGetLastError() << std::endl;
        freeaddrinfo(result);
        WSACleanup();
        return 1;
    }

    wchar_t addressString[256]; // Assuming a maximum length of 256 characters for the address
    DWORD addressStringLength = sizeof(addressString) / sizeof(addressString[0]);
    WSAAddressToStringW(result->ai_addr, (int)result->ai_addrlen, 0, addressString, &addressStringLength);
    std::wcout << L"Binding to " << addressString << std::endl;

    iResult = bind(ListenSocket, result->ai_addr, (int)result->ai_addrlen);
    if (iResult == SOCKET_ERROR) {
        std::cout << "bind failed with error: " << WSAGetLastError() << std::endl;
        freeaddrinfo(result);
        closesocket(ListenSocket);
        WSACleanup();
        return 1;
    }

    // Initialisation de la bibliothèque COM
    HRESULT hr = CoInitializeEx(NULL, COINIT_APARTMENTTHREADED | COINIT_DISABLE_OLE1DDE);

    // Creation du COM IMMDeviceEnumerator
    IMMDeviceEnumerator* pEnumerator = NULL;
    hr = CoCreateInstance(
        CLSID_MMDeviceEnumerator,
        NULL,
        CLSCTX_ALL,
        IID_IMMDeviceEnumerator,
        (void**)&pEnumerator
    );

    // Recuperation de l'endpoint audio par defaut
    IMMDevice* pDevice = NULL;
    pEnumerator->GetDefaultAudioEndpoint(eRender, eConsole, &pDevice);

    // Recuperation de l'interface de controle du volume
    IAudioEndpointVolume* pAudioEndpointVolume = NULL;
    pDevice->Activate(IID_IAudioEndpointVolume, CLSCTX_ALL, NULL, (void**)&pAudioEndpointVolume);

    while (true) {
        float volume = 0.0;
        recvfrom(ListenSocket, (char*)&volume, sizeof(volume), 0, NULL, NULL);
        printf("Setting volume to: %f\n", volume);
        // Changement du volume
        pAudioEndpointVolume->SetMasterVolumeLevelScalar(volume, NULL);
    }

    // Clean
    closesocket(ListenSocket);
    WSACleanup();

    return 0;
}