"""Shared SVG icon markup used by the Humbug UI."""

from typing import Final


HUMBUG_ICON_PACK: Final[dict[str, str]] = {
    "folder": '''
        <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 100 100">
            <path d="M10 25 C10 25 35 25 40 25 C45 25 47 15 50 15 C53 15 90 15 90 15 L90 85 L10 85 L10 25"
                fill="folderColor" stroke="none"/>
        </svg>
    ''',
    "folder_breadcrumb": '''
        <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 100 100">
            <path d="M10 25 C10 25 35 25 40 25 C45 25 47 15 50 15 C53 15 90 15 90 15 L90 85 L10 85 L10 25"
                fill="folderColor" stroke="none"/>
        </svg>
    ''',
    "folder_open": '''
        <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 100 100">
            <path d="M10 28 L38 28 C43 28 45 20 50 20 L90 20 L90 82 L10 82 Z"
                fill="none" stroke="folderColor" stroke-width="8" stroke-linejoin="round"/>
            <path d="M10 82 L28 43 L92 43 L78 82 Z"
                fill="none" stroke="folderColor" stroke-width="8" stroke-linejoin="round"/>
        </svg>
    ''',
    "folder_root": '''
        <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 100 100">
            <path d="M10 25 C10 25 35 25 40 25 C45 25 47 15 50 15 C53 15 90 15 90 15 L90 85 L10 85 L10 25"
                fill="folderColor" stroke="none"/>
            <path d="M50 35 L63 50 L56 50 L56 68 L44 68 L44 50 L37 50 Z"
                fill="folderColor" stroke="none" opacity="0.55"/>
        </svg>
    ''',
    "file": '''
        <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 100 100">
            <path d="M20 10 L60 10 C60 10 60 30 60 30 C60 30 80 30 80 30 L80 90 L20 90 Z"
                fill="none" stroke="currentColor" stroke-width="5"/>
            <path d="M60 10 L80 30" stroke="currentColor" stroke-width="5" fill="none"/>
        </svg>
    ''',
    "conversation": '''
        <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 100 100">
            <path d="M15 15 C15 15 85 15 85 15 C85 15 85 55 85 55 C85 55 70 55 70 55 L50 80 L50 55 C50 55 15 55 15 55 Z"
                fill="none" stroke="currentColor" stroke-width="5"/>
        </svg>
    ''',
    "code": '''
        <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 100 100">
            <path d="M20 10 L60 10 C60 10 60 30 60 30 C60 30 80 30 80 30 L80 90 L20 90 Z"
                fill="none" stroke="currentColor" stroke-width="5"/>
            <path d="M60 10 L80 30" stroke="currentColor" stroke-width="5" fill="none"/>
            <path d="M30 30 L50 30" stroke="accentColor" stroke-width="5"/>
            <path d="M30 50 L70 50" stroke="accentColor" stroke-width="5"/>
            <path d="M30 70 L70 70" stroke="accentColor" stroke-width="5"/>
        </svg>
    ''',
    "text": '''
        <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 100 100">
            <path d="M20 10 L60 10 C60 10 60 30 60 30 C60 30 80 30 80 30 L80 90 L20 90 Z"
                fill="none" stroke="currentColor" stroke-width="5"/>
            <path d="M60 10 L80 30" stroke="currentColor" stroke-width="5" fill="none"/>
            <path d="M30 30 L50 30" stroke="accentColor" stroke-width="5"/>
            <path d="M30 50 L70 50" stroke="accentColor" stroke-width="5"/>
            <path d="M30 70 L70 70" stroke="accentColor" stroke-width="5"/>
        </svg>
    '''
}
