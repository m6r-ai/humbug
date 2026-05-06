from typing import Callable, Dict


def create_app_icon_svg(bg_color: str, text_color: str) -> str:
    """Create the Humbug application icon SVG."""
    return f'''
        <svg xmlns="http://www.w3.org/2000/svg" width="512" height="512" viewBox="0 0 384 384">
            <defs>
                <linearGradient id="humbugIconBg" x1="58" y1="42" x2="318" y2="342" gradientUnits="userSpaceOnUse">
                    <stop offset="0" stop-color="#6ee7ff"/>
                    <stop offset="0.34" stop-color="#6671ff"/>
                    <stop offset="0.68" stop-color="{bg_color}"/>
                    <stop offset="1" stop-color="#22145f"/>
                </linearGradient>
                <linearGradient id="humbugIconShine" x1="88" y1="54" x2="286" y2="260" gradientUnits="userSpaceOnUse">
                    <stop offset="0" stop-color="#ffffff" stop-opacity="0.48"/>
                    <stop offset="0.52" stop-color="#ffffff" stop-opacity="0.1"/>
                    <stop offset="1" stop-color="#ffffff" stop-opacity="0"/>
                </linearGradient>
                <filter id="humbugIconShadow" x="-20%" y="-20%" width="140%" height="140%">
                    <feDropShadow dx="0" dy="16" stdDeviation="18" flood-color="#000000" flood-opacity="0.28"/>
                </filter>
            </defs>
            <rect x="28" y="28" width="328" height="328" rx="78" fill="url(#humbugIconBg)" filter="url(#humbugIconShadow)"/>
            <path d="M63 93 C112 52 246 39 318 95 C279 73 157 73 84 114 C72 121 58 107 63 93 Z"
                fill="url(#humbugIconShine)"/>
            <path d="M116 265 V119 M268 119 V265 M116 192 H268" fill="none"
                stroke="#120f33" stroke-width="52" stroke-linecap="round" stroke-linejoin="round" opacity="0.18"/>
            <path d="M116 265 V119 M268 119 V265 M116 192 H268" fill="none"
                stroke="{text_color}" stroke-width="42" stroke-linecap="round" stroke-linejoin="round"/>
            <circle cx="91" cy="91" r="15" fill="{text_color}" opacity="0.92"/>
            <circle cx="293" cy="293" r="15" fill="{text_color}" opacity="0.92"/>
            <path d="M94 91 C147 70 228 70 290 93 M94 293 C147 314 228 314 290 291"
                fill="none" stroke="{text_color}" stroke-width="9" stroke-linecap="round" opacity="0.46"/>
        </svg>
    '''


def _line_icon_svg(body: str, color: str) -> str:
    return f'''
        <svg viewBox="0 0 64 64" xmlns="http://www.w3.org/2000/svg" fill="none">
            <g stroke="{color}" stroke-width="4.75" stroke-linecap="round" stroke-linejoin="round">
                {body}
            </g>
        </svg>
    '''


def _filled_icon_svg(body: str, color: str) -> str:
    return f'''
        <svg viewBox="0 0 64 64" xmlns="http://www.w3.org/2000/svg">
            <g stroke="{color}" fill="{color}" stroke-width="4.75" stroke-linecap="round" stroke-linejoin="round">
                {body}
            </g>
        </svg>
    '''


def _icon_bodies() -> Dict[str, str]:
    return {
        "arrow-right": '<path d="M25 16 L41 32 L25 48"/>',
        "arrow-left": '<path d="M39 16 L23 32 L39 48"/>',
        "arrow-up": '<path d="M16 39 L32 23 L48 39"/>',
        "arrow-down": '<path d="M16 25 L32 41 L48 25"/>',
        "close": '<path d="M19 19 L45 45"/><path d="M45 19 L19 45"/>',
        "check": '<path d="M15 33 L27 45 L50 19"/>',
        "expand-right": '<path d="M24 14 L42 32 L24 50"/>',
        "expand-left": '<path d="M40 14 L22 32 L40 50"/>',
        "expand-down": '<path d="M14 24 L32 42 L50 24"/>',
        "conversation": (
            '<path d="M12 14 H42 C49 14 54 19 54 26 V35 C54 42 49 47 42 47 H32 L18 56 V47 H12 '
            'C6 47 4 43 4 36 V26 C4 19 6 14 12 14 Z"/>'
            '<path d="M20 29 H44"/><path d="M20 38 H34"/>'
        ),
        "editor": '<path d="M13 49 L18 36 L43 11 L53 21 L28 46 Z"/><path d="M38 16 L48 26"/><path d="M18 36 L28 46"/><path d="M12 53 H52"/>',
        "files": '<path d="M8 20 H24 L30 26 H56 V49 C56 53 53 56 49 56 H15 C11 56 8 53 8 49 Z"/><path d="M8 30 H56"/><path d="M16 40 H39"/>',
        "search": (
            '<circle cx="27" cy="27" r="14"/>'
            '<path d="M38 38 L53 53"/>'
            '<path d="M22 25 C24 22 29 21 33 24"/>'
            '<path d="M45 11 V17"/><path d="M42 14 H48"/>'
        ),
        "log": '<path d="M9 35 H18 L24 18 L35 48 L41 35 H55"/><path d="M9 52 H55"/><path d="M9 12 H55"/>',
        "shell": '<rect x="8" y="12" width="48" height="34" rx="7"/><path d="M18 24 L27 32 L18 40"/><path d="M33 40 H46"/><path d="M24 54 H40"/><path d="M32 46 V54"/>',
        "terminal": '<rect x="8" y="12" width="48" height="40" rx="7"/><path d="M18 25 L27 32 L18 39"/><path d="M34 39 H46"/>',
        "preview": '<path d="M17 8 H36 L49 21 V56 H17 Z"/><path d="M36 8 V21 H49"/><path d="M24 34 C29 28 37 28 42 34 C37 40 29 40 24 34 Z"/><circle cx="33" cy="34" r="3"/>',
        "diff": '<circle cx="18" cy="16" r="5"/><circle cx="18" cy="48" r="5"/><circle cx="46" cy="16" r="5"/><path d="M18 21 V43"/><path d="M46 21 V31 C46 41 39 48 24 48"/>',
        "info": '<circle cx="32" cy="32" r="23"/><path d="M32 29 V45"/><path d="M32 19 H32.2"/>',
        "warning": '<path d="M32 10 L58 54 H6 Z"/><path d="M32 25 V37"/><path d="M32 47 H32.2"/>',
        "critical": '<path d="M22 7 H42 L57 22 V42 L42 57 H22 L7 42 V22 Z"/><path d="M32 22 V37"/><path d="M32 47 H32.2"/>',
        "question": '<circle cx="32" cy="32" r="23"/><path d="M25 25 C26 20 30 18 34 19 C39 20 42 24 41 29 C40 34 34 35 32 39 V41"/><path d="M32 48 H32.2"/>',
        "save": '<path d="M13 13 H43 L51 21 V51 H13 Z"/><path d="M22 13 V28 H42 V13"/><path d="M22 51 V38 H42 V51"/>',
        "cog": '<path d="M30 8 H38 L40 17 C42 18 44 19 46 21 L55 18 L59 25 L52 32 C52 34 52 36 51 38 L58 45 L54 52 L45 49 C43 51 41 52 38 53 L36 62 H28 L26 53 C24 52 22 51 20 49 L11 52 L7 45 L14 38 C13 36 12 34 12 32 L5 25 L9 18 L18 21 C20 19 22 18 24 17 Z"/><circle cx="32" cy="35" r="9"/>',
        "copy": '<rect x="21" y="9" width="31" height="31" rx="4"/><rect x="12" y="24" width="31" height="31" rx="4"/>',
        "fork": '<circle cx="18" cy="14" r="5"/><circle cx="46" cy="50" r="5"/><circle cx="18" cy="50" r="5"/><path d="M18 19 V33 C18 42 25 50 41 50"/><path d="M18 19 V50"/>',
        "delete": '<path d="M11 18 H53"/><path d="M23 18 L26 10 H38 L41 18"/><path d="M18 18 L21 55 H43 L46 18"/><path d="M28 29 V45"/><path d="M36 29 V45"/>',
        "edit": '<path d="M13 49 L18 36 L42 12 L52 22 L28 46 Z"/><path d="M37 17 L47 27"/><path d="M12 52 H52"/>',
        "submit": '<path d="M32 53 V12"/><path d="M14 30 L32 12 L50 30"/>',
        "paperclip": '<path d="M48 28 L29 47 C23 53 14 53 10 48 C5 43 6 35 12 29 L38 13 C42 9 49 10 53 15 C57 20 56 27 51 32 L27 48 C24 50 20 49 18 46 C16 43 17 40 20 38 L42 21"/>',
        "minimize": '<path d="M14 34 H50"/>',
        "maximize": '<rect x="14" y="14" width="36" height="36" rx="2"/>',
        "restore": '<rect x="22" y="10" width="32" height="32" rx="2"/><path d="M10 24 V54 H40"/>',
        "update": '<path d="M51 24 C48 15 41 10 32 10 C22 10 14 17 12 27"/><path d="M13 13 V27 H27"/><path d="M13 40 C16 49 23 54 32 54 C42 54 50 47 52 37"/><path d="M51 51 V37 H37"/>',
        "find-hidden": '<path d="M6 32 C14 18 23 12 32 12 C41 12 50 18 58 32 C50 46 41 52 32 52 C23 52 14 46 6 32 Z"/><circle cx="32" cy="32" r="8"/>',
    }


def _filled_icon_bodies() -> Dict[str, str]:
    return {
        "stop": '<circle cx="32" cy="32" r="23" fill="none"/><rect x="23" y="23" width="18" height="18" rx="2" stroke="none"/>',
        "find-match-case": '<text x="7" y="46" font-family="Arial, sans-serif" font-size="34" font-weight="700" stroke="none">Aa</text>',
        "find-whole-word": '<path d="M10 16 V48" fill="none"/><path d="M54 16 V48" fill="none"/><text x="18" y="44" font-family="Arial, sans-serif" font-size="28" font-weight="700" stroke="none">ab</text>',
        "find-regexp": '<circle cx="15" cy="39" r="4" stroke="none"/><path d="M41 17 V43" fill="none"/><path d="M30 24 L52 37" fill="none"/><path d="M52 24 L30 37" fill="none"/><path d="M8 53 H56" fill="none"/>',
    }


def _mindspace_tree_icon_svgs() -> Dict[str, str]:
    return {
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
    }


def write_icon_pack(write_icon: Callable[[str, str], None], suffix: str, color: str, prefix: str = "") -> None:
    """Write a themed set of first-party Humbug icons."""
    for name, body in _icon_bodies().items():
        write_icon(f"{prefix}{name}-{suffix}.svg", _line_icon_svg(body, color))

    for name, body in _filled_icon_bodies().items():
        write_icon(f"{prefix}{name}-{suffix}.svg", _filled_icon_svg(body, color))


def create_icon_svg(name: str, color: str) -> str:
    """Create a themed SVG for one icon by name."""
    if name in _filled_icon_bodies():
        return _filled_icon_svg(_filled_icon_bodies()[name], color)

    return _line_icon_svg(_icon_bodies()[name], color)


def create_mindspace_tree_icon_svg(name: str) -> str:
    """Create a dynamic mindspace tree SVG by name."""
    return _mindspace_tree_icon_svgs()[name]
