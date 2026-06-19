import struct

from docx.docx_ast_serialiser import _read_image_dimensions


def _make_png(width: int, height: int) -> bytes:
    """Build a minimal PNG header sufficient for dimension reading."""
    signature = b'\x89PNG\r\n\x1a\n'
    ihdr_data = struct.pack('>II', width, height) + b'\x08\x02\x00\x00\x00'
    ihdr_chunk = b'\x00\x00\x00\rIHDR' + ihdr_data + b'\x00\x00\x00\x00'
    return signature + ihdr_chunk


def _make_gif(width: int, height: int) -> bytes:
    """Build a minimal GIF header sufficient for dimension reading."""
    return b'GIF89a' + struct.pack('<HH', width, height) + b'\x00' * 10


def _make_jpeg(width: int, height: int) -> bytes:
    """Build a minimal JPEG with a single SOF0 marker."""
    sof0_payload = struct.pack('>BHHB', 8, height, width, 3) + b'\x00' * 9
    sof0_len = struct.pack('>H', 2 + len(sof0_payload))
    return b'\xff\xd8\xff\xc0' + sof0_len + sof0_payload


def _make_webp_lossy(width: int, height: int) -> bytes:
    """Build a minimal lossy WEBP (VP8) header sufficient for dimension reading."""
    w_bits = struct.pack('<H', (width - 1) & 0x3FFF)
    h_bits = struct.pack('<H', (height - 1) & 0x3FFF)
    vp8_payload = b'\x00' * 6 + w_bits + h_bits
    vp8_chunk = b'VP8 ' + struct.pack('<I', len(vp8_payload)) + vp8_payload
    total = 4 + len(vp8_chunk)
    return b'RIFF' + struct.pack('<I', total) + b'WEBP' + vp8_chunk


def _make_webp_lossless(width: int, height: int) -> bytes:
    """Build a minimal lossless WEBP (VP8L) header sufficient for dimension reading."""
    bits = ((width - 1) & 0x3FFF) | (((height - 1) & 0x3FFF) << 14)
    vp8l_payload = b'\x2f' + struct.pack('<I', bits) + b'\x00' * 4
    vp8l_chunk = b'VP8L' + struct.pack('<I', len(vp8l_payload)) + vp8l_payload
    total = 4 + len(vp8l_chunk)
    return b'RIFF' + struct.pack('<I', total) + b'WEBP' + vp8l_chunk


def _make_webp_extended(width: int, height: int) -> bytes:
    """Build a minimal extended WEBP (VP8X) header sufficient for dimension reading."""
    w3 = [(width - 1) >> (8 * i) & 0xFF for i in range(3)]
    h3 = [(height - 1) >> (8 * i) & 0xFF for i in range(3)]
    vp8x_payload = b'\x00' * 4 + bytes(w3) + bytes(h3)
    vp8x_chunk = b'VP8X' + struct.pack('<I', len(vp8x_payload)) + vp8x_payload
    total = 4 + len(vp8x_chunk)
    return b'RIFF' + struct.pack('<I', total) + b'WEBP' + vp8x_chunk


class TestReadImageDimensions:
    def test_png(self):
        assert _read_image_dimensions(_make_png(800, 600)) == (800, 600)

    def test_png_non_square(self):
        assert _read_image_dimensions(_make_png(1920, 1080)) == (1920, 1080)

    def test_gif(self):
        assert _read_image_dimensions(_make_gif(320, 240)) == (320, 240)

    def test_jpeg(self):
        assert _read_image_dimensions(_make_jpeg(640, 480)) == (640, 480)

    def test_webp_lossy(self):
        assert _read_image_dimensions(_make_webp_lossy(1024, 768)) == (1024, 768)

    def test_webp_lossless(self):
        assert _read_image_dimensions(_make_webp_lossless(512, 384)) == (512, 384)

    def test_webp_extended(self):
        assert _read_image_dimensions(_make_webp_extended(2048, 1536)) == (2048, 1536)

    def test_unknown_format_returns_none(self):
        assert _read_image_dimensions(b'\x00\x01\x02\x03' * 10) is None

    def test_too_short_returns_none(self):
        assert _read_image_dimensions(b'\x89PNG') is None

    def test_empty_returns_none(self):
        assert _read_image_dimensions(b'') is None
