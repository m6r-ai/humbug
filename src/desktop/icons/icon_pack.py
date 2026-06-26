"""Shared SVG icon markup used by the Humbug UI."""

from typing import Final, Dict, Tuple


def app_icon_svg(text_color: str, bg_start: str, bg_end: str) -> str:
    """Return the application icon SVG with a gradient container background."""
    return APP_ICON_TEMPLATE.format(
        text_color=text_color, bg_start=bg_start, bg_end=bg_end
    )


def theme_icon_svg(icon_name: str, color: str) -> str:
    """Return a single-colour themed SVG icon."""
    return THEME_ICON_PACK[icon_name].format(color=color)


def update_icon_svg(update_color: str) -> str:
    """Return the update icon SVG."""
    return THEME_ICON_PACK["update"].format(update_color=update_color)


def active_inactive_icon_names() -> Tuple[str, ...]:
    """Return the themed icons generated for active, inactive, and bright states."""
    return ACTIVE_INACTIVE_THEME_ICONS


APP_ICON_TEMPLATE: Final[str] = '''
    <svg xmlns="http://www.w3.org/2000/svg" width="512" height="512" viewBox="0 0 512 512">
        <defs>
            <linearGradient id="bgGrad" x1="0%" y1="0%" x2="100%" y2="100%">
                <stop offset="0%"   stop-color="{bg_start}"/>
                <stop offset="100%" stop-color="{bg_end}"/>
            </linearGradient>
        </defs>
        <rect x="0" y="0" width="512" height="512" rx="64" ry="64" fill="url(#bgGrad)"/>
        <rect x="72"  y="72" width="112" height="368" rx="56" ry="56" fill="{text_color}"/>
        <rect x="328" y="72" width="112" height="368" rx="56" ry="56" fill="{text_color}"/>
        <rect x="184" y="200" width="144" height="112"               fill="{text_color}"/>
    </svg>
'''


ACTIVE_INACTIVE_THEME_ICONS: Final[Tuple[str, ...]] = (
    "close",
    "conversation",
    "editor",
    "files",
    "search",
    "log",
    "shell",
    "terminal",
    "preview",
    "diff",
    "usage",
    "tab-overview",
    "tab-carousel",
)


THEME_ICON_PACK: Final[Dict[str, str]] = {
    "arrow-right": '''
        <svg viewBox="0 0 64 64" xmlns="http://www.w3.org/2000/svg">
            <path stroke="{color}" stroke-width="6" fill="none" d="M24,16 L40,32 L24,48"/>
        </svg>
    ''',
    "arrow-left": '''
        <svg viewBox="0 0 64 64" xmlns="http://www.w3.org/2000/svg">
            <path stroke="{color}" stroke-width="6" fill="none" d="M40,16 L24,32 L40,48"/>
        </svg>
    ''',
    "arrow-up": '''
        <svg viewBox="0 0 64 64" xmlns="http://www.w3.org/2000/svg">
            <path stroke="{color}" stroke-width="6" fill="none" d="M16,40 L32,24 L48,40"/>
        </svg>
    ''',
    "arrow-down": '''
        <svg viewBox="0 0 64 64" xmlns="http://www.w3.org/2000/svg">
            <path stroke="{color}" stroke-width="6" fill="none" d="M16,24 L32,40 L48,24"/>
        </svg>
    ''',
    "close": '''
        <svg viewBox="0 0 64 64" xmlns="http://www.w3.org/2000/svg">
            <path stroke="{color}" stroke-width="6" fill="none"
                d="M16,16 L48,48 M48,16 L16,48"/>
        </svg>
    ''',
    "check": '''
        <svg viewBox="0 0 64 64" xmlns="http://www.w3.org/2000/svg">
            <path stroke="{color}" stroke-width="6" fill="none"
                d="M16,32 L28,44 L48,20" stroke-linecap="round" stroke-linejoin="round"/>
        </svg>
    ''',
    "expand-right": '''
        <svg viewBox="0 0 64 64" xmlns="http://www.w3.org/2000/svg">
            <path stroke="{color}" stroke-width="6" fill="none" d="M20,8 L44,32 L20,56"/>
        </svg>
    ''',
    "expand-left": '''
        <svg viewBox="0 0 64 64" xmlns="http://www.w3.org/2000/svg">
            <path stroke="{color}" stroke-width="6" fill="none" d="M44,8 L20,32 L44,56"/>
        </svg>
    ''',
    "expand-down": '''
        <svg viewBox="0 0 64 64" xmlns="http://www.w3.org/2000/svg">
            <path stroke="{color}" stroke-width="6" fill="none" d="M8,20 L32,44 L56,20"/>
        </svg>
    ''',
    "conversation": '''
        <svg viewBox="0 0 24 24" fill="none" xmlns="http://www.w3.org/2000/svg">
            <path d="M8 10H8.01" stroke="{color}" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"/>
            <path d="M12 10H12.01" stroke="{color}" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"/>
            <path d="M16 10H16.01" stroke="{color}" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"/>
            <path d="M21 13V7C21 5.11438 21 4.17157 20.4142 3.58579C19.8284 3 18.8856 3 17 3H7C5.11438 3
                4.17157 3 3.58579 3.58579C3 4.17157 3 5.11438 3 7V13C3 14.8856 3 15.8284 3.58579 16.4142C4.17157
                17 5.11438 17 7 17H7.5C7.77614 17 8 17.2239 8 17.5V20V20.1499C8 20.5037 8.40137 20.7081
                8.6875 20.5L13.0956 17.2941C13.3584 17.103 13.675 17 14 17H17C18.8856 17 19.8284 17 20.4142
                16.4142C21 15.8284 21 14.8856 21 13Z" stroke="{color}" stroke-width="2" stroke-linejoin="round"/>
        </svg>
    ''',
    "editor": '''
        <svg viewBox="0 0 24 24" fill="none" xmlns="http://www.w3.org/2000/svg">
            <path d="M13 21H21" stroke="{color}" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"/>
            <path d="M20.0651 7.39423L7.09967 20.4114C6.72438 20.7882 6.21446 21 5.68265 21H4.00383C3.44943
                21 3 20.5466 3 19.9922V18.2987C3 17.7696 3.20962 17.2621 3.58297 16.8873L16.5517 3.86681C19.5632
                1.34721 22.5747 4.87462 20.0651 7.39423Z" stroke="{color}" stroke-width="2"
                stroke-linecap="round" stroke-linejoin="round"/>
            <path d="M15.3097 5.30981L18.7274 8.72755" stroke="{color}" stroke-width="2"
                stroke-linecap="round" stroke-linejoin="round"/>
        </svg>
    ''',
    "files": '''
        <svg viewBox="0 0 24 24" fill="none" xmlns="http://www.w3.org/2000/svg">
            <path d="M4 7C4 5.89543 4.89543 5 6 5H10L12 7H18C19.1046 7 20 7.89543 20 9V17C20 18.1046 19.1046 19 18 19H6
                C4.89543 19 4 18.1046 4 17V7Z"
                stroke="{color}" stroke-width="2" stroke-linejoin="round"/>
            <path d="M4 10H20" stroke="{color}" stroke-width="2" stroke-linecap="round"/>
        </svg>
    ''',
    "search": '''
        <svg viewBox="0 0 24 24" fill="none" xmlns="http://www.w3.org/2000/svg">
            <path d="M15 15L21 21" stroke="{color}" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"/>
            <path d="M17 10C17 13.866 13.866 17 10 17C6.13401 17 3 13.866 3 10C3 6.13401 6.13401 3 10 3C13.866 3 17 6.13401 17 10Z" stroke="{color}" stroke-width="2"/>
        </svg>
    ''',
    "log": '''
        <svg viewBox="0 0 24 24" fill="none" xmlns="http://www.w3.org/2000/svg">
            <path d="M5 12H7.75044C7.89947 12 8.03179 11.9046 8.07892 11.7632V11.7632L9.875 6.375V6.375C9.91626
                6.25122 10.0918 6.25238 10.1364 6.375V6.375L13.875 16.6562L13.885 16.6837C13.9253 16.7946 14.0812
                16.797 14.125 16.6875V16.6875L15.8841 12.2898V12.2898C15.9541 12.1148 16.1236 12 16.3122 12H19"
                stroke="{color}" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"/>
        </svg>
    ''',
    "shell": '''
        <svg viewBox="0 0 24 24" fill="none" xmlns="http://www.w3.org/2000/svg">
            <path d="M3 7C3 5.11438 3 4.17157 3.58579 3.58579C4.17157 3 5.11438 3 7 3H12H17C18.8856 3 19.8284
                3 20.4142 3.58579C21 4.17157 21 5.11438 21 7V10V13C21 14.8856 21 15.8284 20.4142 16.4142C19.8284
                17 18.8856 17 17 17H12H7C5.11438 17 4.17157 17 3.58579 16.4142C3 15.8284 3 14.8856 3 13V10V7Z"
                stroke="{color}" stroke-width="2" stroke-linejoin="round"/>
            <path d="M7 21H17" stroke="{color}" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"/>
            <path d="M12 17V21" stroke="{color}" stroke-width="2" stroke-linecap="round"/>
        </svg>
    ''',
    "terminal": '''
        <svg viewBox="0 0 24 24" fill="none" xmlns="http://www.w3.org/2000/svg">
            <path d="M13 15H16" stroke="{color}" stroke-width="2" stroke-linecap="round"/>
            <path d="M8 15L10.5 12.5V12.5C10.7761 12.2239 10.7761 11.7761 10.5 11.5V11.5L8 9"
                stroke="{color}" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"/>
            <path d="M3 8C3 6.11438 3 5.17157 3.58579 4.58579C4.17157 4 5.11438 4 7 4H12H17C18.8856 4
                19.8284 4 20.4142 4.58579C21 5.17157 21 6.11438 21 8V12V16C21 17.8856 21 18.8284 20.4142
                19.4142C19.8284 20 18.8856 20 17 20H12H7C5.11438 20 4.17157 20 3.58579 19.4142C3 18.8284
                3 17.8856 3 16V12V8Z" stroke="{color}" stroke-width="2" stroke-linejoin="round"/>
        </svg>
    ''',
    "preview": '''
        <svg viewBox="0 0 24 24" fill="none" xmlns="http://www.w3.org/2000/svg">
            <path d="M17.8284 6.82843C18.4065 7.40649 18.6955 7.69552 18.8478 8.06306C19 8.4306 19 8.83935
                19 9.65685L19 17C19 18.8856 19 19.8284 18.4142 20.4142C17.8284 21 16.8856 21 15 21H9C7.11438
                21 6.17157 21 5.58579 20.4142C5 19.8284 5 18.8856 5 17L5 7C5 5.11438 5 4.17157 5.58579
                3.58579C6.17157 3 7.11438 3 9 3H12.3431C13.1606 3 13.5694 3 13.9369 3.15224C14.3045 3.30448
                14.5935 3.59351 15.1716 4.17157L17.8284 6.82843Z" stroke="{color}"
                stroke-width="2" stroke-linejoin="round"/>
            <path d="M9 6L11 6" stroke="{color}" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"/>
            <path d="M10 9L12 9" stroke="{color}" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"/>
            <path d="M9 12L11 12" stroke="{color}" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"/>
            <path d="M10 15L12 15" stroke="{color}" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"/>
        </svg>
    ''',
    "diff": '''
        <svg viewBox="0 0 24 24" fill="none" xmlns="http://www.w3.org/2000/svg">
            <path d="M3 6C3 7.65685 4.34315 9 6 9C7.65685 9 9 7.65685 9 6C9 4.34315 7.65685 3 6 3C4.34315 3 3 4.34315 3 6Z"
                stroke="{color}" stroke-width="2"/>
            <path d="M3 18C3 19.6569 4.34315 21 6 21C7.65685 21 9 19.6569 9 18C9 16.3431 7.65685 15 6 15C4.34315 15 3 16.3431 3 18Z"
                stroke="{color}" stroke-width="2"/>
            <path d="M15 6C15 7.65685 16.3431 9 18 9C19.6569 9 21 7.65685 21 6C21 4.34315 19.6569 3 18 3C16.3431 3 15 4.34315 15 6Z"
                stroke="{color}" stroke-width="2"/>
            <path d="M6 15V9" stroke="{color}" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"/>
            <path d="M18 9V12.3242C18 16.9982 16.9424 18 12.008 18H9" stroke="{color}" stroke-width="2" stroke-linecap="round"/>
        </svg>
    ''',
    "usage": '''
        <svg viewBox="0 0 24 24" fill="none" xmlns="http://www.w3.org/2000/svg">
            <path d="M12 2C6.47715 2 2 6.47715 2 12C2 17.5228 6.47715 22 12 22C17.5228 22 22 17.5228 22 12C22 6.47715 17.5228 2 12 2Z"
                stroke="{color}" stroke-width="2"/>
            <path d="M12 6V8M12 16V18" stroke="{color}" stroke-width="2" stroke-linecap="round"/>
            <path d="M9 9.5C9 8.67157 9.67157 8 10.5 8H13C14.1046 8 15 8.89543 15 10C15 11.1046 14.1046 12 13 12H11C9.89543 12 9 12.8954 9 14C9 15.1046 9.89543 16 11 16H13.5C14.3284 16 15 15.3284 15 14.5"
                stroke="{color}" stroke-width="2" stroke-linecap="round"/>
        </svg>
    ''',
    "info": '''
        <svg width="800px" height="800px" viewBox="0 0 24 24" fill="none" xmlns="http://www.w3.org/2000/svg">
            <path d="M12 7C9.23858 7 7 9.23858 7 12C7 13.3613 7.54402 14.5955 8.42651 15.4972C8.77025 15.8484 9.05281 16.2663
                9.14923 16.7482L9.67833 19.3924C9.86537 20.3272 10.6862 21 11.6395 21H12.3605C13.3138 21 14.1346 20.3272
                14.3217 19.3924L14.8508 16.7482C14.9472 16.2663 15.2297 15.8484 15.5735 15.4972C16.456 14.5955 17
                13.3613 17 12C17 9.23858 14.7614 7 12 7Z" stroke="{color}" stroke-width="2"/>
            <path d="M12 4V3" stroke="{color}" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"/>
            <path d="M18 6L19 5" stroke="{color}" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"/>
            <path d="M20 12H21" stroke="{color}" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"/>
            <path d="M4 12H3" stroke="{color}" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"/>
            <path d="M5 5L6 6" stroke="{color}" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"/>
            <path d="M10 17H14" stroke="{color}" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"/>
        </svg>
    ''',
    "warning": '''
        <svg width="800px" height="800px" viewBox="0 0 24 24" fill="none" xmlns="http://www.w3.org/2000/svg">
            <path d="M12 10V13" stroke="{color}" stroke-width="2" stroke-linecap="round"/>
            <path d="M12 16V15.9888" stroke="{color}" stroke-width="2" stroke-linecap="round"/>
            <path d="M10.2518 5.147L3.6508 17.0287C2.91021 18.3618 3.87415 20 5.39912 20H18.6011C20.126 20 21.09
                18.3618 20.3494 17.0287L13.7484 5.147C12.9864 3.77538 11.0138 3.77538 10.2518 5.147Z"
                stroke="{color}" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"/>
        </svg>
    ''',
    "critical": '''
        <svg width="800px" height="800px" viewBox="0 0 24 24" fill="none" xmlns="http://www.w3.org/2000/svg">
            <path d="M3 9.22843V14.7716C3 15.302 3.21071 15.8107 3.58579 16.1858L7.81421 20.4142C8.18929 20.7893 8.69799
                21 9.22843 21H14.7716C15.302 21 15.8107 20.7893 16.1858 20.4142L20.4142 16.1858C20.7893 15.8107 21
                15.302 21 14.7716V9.22843C21 8.69799 20.7893 8.18929 20.4142 7.81421L16.1858 3.58579C15.8107 3.21071
                15.302 3 14.7716 3H9.22843C8.69799 3 8.18929 3.21071 7.81421 3.58579L3.58579 7.81421C3.21071 8.18929
                3 8.69799 3 9.22843Z" stroke="{color}" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"/>
            <path d="M12 8V13" stroke="{color}" stroke-width="2" stroke-linecap="round"/>
            <path d="M12 16V15.9888" stroke="{color}" stroke-width="2" stroke-linecap="round"/>
        </svg>
    ''',
    "question": '''
        <svg width="800px" height="800px" viewBox="0 0 24 24" fill="none" xmlns="http://www.w3.org/2000/svg">
            <path d="M21 12C21 16.9706 16.9706 21 12 21C7.02944 21 3 16.9706 3 12C3 7.02944 7.02944 3 12
                3C16.9706 3 21 7.02944 21 12Z" stroke="{color}" stroke-width="2"/>
            <path d="M10.5 8.67709C10.8665 8.26188 11.4027 8 12 8C13.1046 8 14 8.89543 14 10C14 10.9337
                13.3601 11.718 12.4949 11.9383C12.2273 12.0064 12 12.2239 12 12.5V12.5V13"
                stroke="{color}" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"/>
            <path d="M12 16H12.01" stroke="{color}" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"/>
        </svg>
    ''',
    "save": '''
        <svg width="64" height="64" viewBox="0 0 64 64" xmlns="http://www.w3.org/2000/svg">
            <path stroke="{color}" stroke-width="6" fill="none" d="M8,40, L8,56 L56,56 L56,40"/>
            <path stroke="{color}" stroke-width="6" fill="none" d="M32,8, L32,40"/>
            <path stroke="{color}" stroke-width="6" fill="none" d="M20,28, L32,40, L44,28"/>
        </svg>
    ''',
    "floppy": '''
        <svg width="64" height="64" viewBox="0 0 64 64" xmlns="http://www.w3.org/2000/svg">
            <path stroke="{color}" stroke-width="6" fill="none" stroke-linejoin="miter"
                d="M8,8 L44,8 L56,20 L56,56 L8,56 Z"/>
            <path stroke="{color}" stroke-width="6" fill="none" stroke-linejoin="miter" d="M18,8 L18,24 L42,24 L42,8"/>
            <rect x="20" y="36" width="24" height="16" stroke="{color}" stroke-width="6" fill="none"/>
        </svg>
    ''',
    "cog": '''
        <svg width="64" height="64" viewBox="0 0 64 64" xmlns="http://www.w3.org/2000/svg">
            <path stroke="{color}" stroke-width="4" fill="none" stroke-linejoin="miter"
                d="M24,16 L31,14 L33,6 L44,9 L42,17 L47,22 L55,20 L58,31 L50,33 L48,40 L54,46 L46,54
                L40,48 L33,50 L31,58 L20,55 L22,47 L17,42 L9,44 L6,33 L14,31 L16,24 L10,18 L18,10 Z
                M40,32 A8,8 0 1,1 24,32 A8,8 0 1,1 40,32 Z"/>
        </svg>
    ''',
    "copy": '''
        <svg width="64" height="64" viewBox="0 0 64 64" xmlns="http://www.w3.org/2000/svg">
            <rect x="22" y="10" width="32" height="32" stroke="{color}" stroke-width="6" fill="none"/>
            <path stroke="{color}" stroke-width="6" fill="none" d="M22,22, L10,22 L10,54 L42,54 L42,42"/>
        </svg>
    ''',
    "fork": '''
        <svg width="64" height="64" viewBox="0 0 64 64" xmlns="http://www.w3.org/2000/svg">
            <path stroke="{color}" stroke-width="6" fill="none"
                d="M32,8 L32,32 M32,32 L8,56 L8,40 M8,56 L24,56 M32,32 L56,56 L56,40 M56,56 L40,56"/>
        </svg>
    ''',
    "delete": '''
        <svg viewBox="0 0 64 64" xmlns="http://www.w3.org/2000/svg">
            <path stroke="{color}" stroke-width="6" fill="none"
                d="M12.5,20 L12.5,56 L51.5,56 L51.5,20 M4,20 L60,20 M18,20 L24,8 L40,8 L46,20"/>
            <path stroke="{color}" stroke-width="6" fill="none" d="M25.5,28 L25.5,46 M38.5,28 L38.5,46"/>
        </svg>
    ''',
    "edit": '''
        <svg width="64" height="64" viewBox="0 0 64 64" xmlns="http://www.w3.org/2000/svg">
            <path stroke="{color}" stroke-width="6" fill="none" d="M8,56 L56,56 M40,12 L52,24 L24,52 L8,56 L12,40 L40,12 Z"/>
        </svg>
    ''',
    "submit": '''
        <svg viewBox="0 0 64 64" xmlns="http://www.w3.org/2000/svg">
            <path stroke="{color}" stroke-width="6" fill="none" d="M8,32 L32,8 L56,32"/>
            <path stroke="{color}" stroke-width="6" fill="none" d="M32,56 L32,8"/>
        </svg>
    ''',
    "stop": '''
        <svg viewBox="0 0 64 64" xmlns="http://www.w3.org/2000/svg">
            <circle cx="32" cy="32" r="28" fill="none" stroke="{color}" stroke-width="6"/>
            <rect x="20" y="20" width="24" height="24" fill="{color}" stroke="none"/>
        </svg>
    ''',
    "paperclip": '''
        <svg viewBox="0 0 64 64" xmlns="http://www.w3.org/2000/svg">
            <path d="M56 32 L34 54 C28 60 19 60 13 54 C7 48 7 39 13 33 L39 8 C43 4 49 4 53 8 C57 12 57 18 53 22 L29 46 C27 48 24 48 22 46 C20 44 20 41 22 39 L44 17"
                  fill="none" stroke="{color}" stroke-width="5" stroke-linecap="round" stroke-linejoin="round"/>
        </svg>
    ''',
    "minimize": '''
        <svg viewBox="0 0 64 64" xmlns="http://www.w3.org/2000/svg">
            <line x1="12" y1="32" x2="52" y2="32" stroke="{color}" stroke-width="6" stroke-linecap="round"/>
        </svg>
    ''',
    "maximize": '''
        <svg viewBox="0 0 64 64" xmlns="http://www.w3.org/2000/svg">
            <rect x="12" y="12" width="40" height="40" stroke="{color}" stroke-width="6" fill="none" stroke-linejoin="round"/>
        </svg>
    ''',
    "restore": '''
        <svg viewBox="0 0 64 64" xmlns="http://www.w3.org/2000/svg">
            <rect x="20" y="8" width="36" height="36" stroke="{color}" stroke-width="6" fill="none" stroke-linejoin="round"/>
            <path d="M8,24 L8,56 L40,56 L40,44" stroke="{color}" stroke-width="6" fill="none" stroke-linecap="round" stroke-linejoin="round"/>
        </svg>
    ''',
    "update": '''
        <svg viewBox="0 0 24 24" fill="none" xmlns="http://www.w3.org/2000/svg">
            <path d="M20.5 15C18.9558 18.0448 15.7622 21 12 21C7.14776 21 3.58529 17.5101 3 13"
                stroke="{update_color}" stroke-width="2" stroke-linecap="round"/>
            <path d="M3.5 9C4.89106 5.64934 8.0647 3 12 3C16.7819 3 20.4232 6.48993 21 11"
                stroke="{update_color}" stroke-width="2" stroke-linecap="round"/>
            <path d="M21 21L21 15.6C21 15.2686 20.7314 15 20.4 15V15L15 15"
                stroke="{update_color}" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"/>
            <path d="M9 9L3.6 9V9C3.26863 9 3 8.73137 3 8.4L3 3"
                stroke="{update_color}" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"/>
        </svg>
    ''',
    "find-match-case": '''
        <svg viewBox="0 0 64 64" xmlns="http://www.w3.org/2000/svg">
            <text x="3" y="48" font-family="sans-serif" font-size="48" fill="{color}">Aa</text>
        </svg>
    ''',
    "find-whole-word": '''
        <svg viewBox="0 0 64 64" xmlns="http://www.w3.org/2000/svg">
            <line x1="6" y1="12" x2="6" y2="52" stroke="{color}" stroke-width="5" stroke-linecap="round"/>
            <line x1="58" y1="12" x2="58" y2="52" stroke="{color}" stroke-width="5" stroke-linecap="round"/>
            <text x="16" y="46" font-family="sans-serif" font-size="34" fill="{color}">ab</text>
        </svg>
    ''',
    "find-regexp": '''
        <svg viewBox="0 0 64 64" xmlns="http://www.w3.org/2000/svg">
            <circle cx="14" cy="36" r="5" fill="{color}"/>#
            <line x1="42" y1="16" x2="42" y2="44" stroke="{color}" stroke-width="5" stroke-linecap="round"/>
            <line x1="30" y1="22" x2="54" y2="38" stroke="{color}" stroke-width="5" stroke-linecap="round"/>
            <line x1="54" y1="22" x2="30" y2="38" stroke="{color}" stroke-width="5" stroke-linecap="round"/>
            <line x1="4" y1="54" x2="60" y2="54" stroke="{color}" stroke-width="4" stroke-linecap="round"/>
        </svg>
    ''',
    "find-hidden": '''
        <svg viewBox="0 0 64 64" xmlns="http://www.w3.org/2000/svg">
            <path stroke="{color}" stroke-width="5" fill="none" stroke-linecap="round" stroke-linejoin="round"
                d="M4,32 Q16,12 32,12 Q48,12 60,32 Q48,52 32,52 Q16,52 4,32 Z"/>
            <circle cx="32" cy="32" r="9" stroke="{color}" stroke-width="5" fill="none"/>
        </svg>
    ''',
    "tab-overview": '''
        <svg viewBox="0 0 24 24" fill="none" xmlns="http://www.w3.org/2000/svg">
            <rect x="3" y="3" width="7" height="7" rx="1.5" stroke="{color}" stroke-width="2" stroke-linejoin="round"/>
            <rect x="14" y="3" width="7" height="7" rx="1.5" stroke="{color}" stroke-width="2" stroke-linejoin="round"/>
            <rect x="3" y="14" width="7" height="7" rx="1.5" stroke="{color}" stroke-width="2" stroke-linejoin="round"/>
            <rect x="14" y="14" width="7" height="7" rx="1.5" stroke="{color}" stroke-width="2" stroke-linejoin="round"/>
        </svg>
    ''',
    "tab-carousel": '''
        <svg viewBox="0 0 24 24" fill="none" xmlns="http://www.w3.org/2000/svg">
            <rect x="8" y="3" width="8" height="18" rx="1.5" stroke="{color}" stroke-width="2" stroke-linejoin="round"/>
            <rect x="3" y="6" width="4" height="12" rx="1.5" stroke="{color}" stroke-width="2" stroke-linejoin="round"/>
            <rect x="17" y="6" width="4" height="12" rx="1.5" stroke="{color}" stroke-width="2" stroke-linejoin="round"/>
        </svg>
    ''',
}


MINDSPACE_ICON_PACK: Final[dict[str, str]] = {
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
    ''',
    "blueprint": '''
        <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 100 100">
            <path d="M20 10 L60 10 C60 10 60 30 60 30 C60 30 80 30 80 30 L80 90 L20 90 Z"
                fill="none" stroke="currentColor" stroke-width="5"/>
            <path d="M60 10 L80 30" stroke="currentColor" stroke-width="5" fill="none"/>
            <line x1="20" y1="50" x2="80" y2="50" stroke="currentColor" stroke-width="4"/>
            <line x1="50" y1="10" x2="50" y2="90" stroke="currentColor" stroke-width="4"/>
        </svg>
    '''
}
