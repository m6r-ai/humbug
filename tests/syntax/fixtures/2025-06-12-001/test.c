/*
 * main.c
 *	Opening file for the sinister tool.
 */
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <string.h>
#include <unistd.h>
#include <sys/stat.h>
#include <sys/fcntl.h>

/*
 * 32-bit ELF base types.
 */
typedef uint32_t elf32_addr;
typedef uint16_t elf32_half;
typedef uint32_t elf32_off;
typedef int32_t elf32_sword;
typedef uint32_t elf32_word;

/*
 * ELF header.
 */
struct elf32_hdr {
	unsigned char e_ident[16];
	elf32_half e_type;
	elf32_half e_machine;
	elf32_word e_version;
	elf32_addr e_entry;
	elf32_off e_phoff;
	elf32_off e_shoff;
	elf32_word e_flags;
	elf32_half e_ehsize;
	elf32_half e_phentsize;
	elf32_half e_phnum;
	elf32_half e_shentsize;
	elf32_half e_shnum;
	elf32_half e_shstrndx;
};

/*
 * e_ident field offsets.
 */
#define EI_MAG0 0
#define EI_MAG1 1
#define EI_MAG2 2
#define EI_MAG3 3
#define EI_CLASS 4
#define EI_DATA 5
#define EI_VERSION 6
#define EI_PAD 7

/*
 * EI_DATA field values.
 */
#define ELFDATANONE 0
#define ELFDATA2LSB 1
#define ELFDATA2MSB 2

/*
 * ELF file types (e_type in elf32_hdr).
 */
#define ET_NONE 0
#define ET_REL 1
#define ET_EXEC 2
#define ET_DYN 3
#define ET_CORE 4
#define ET_LOPROC 0xff00
#define ET_HIPROC 0xffff

/*
 * ELF program header.
 */
struct elf32_phdr {
	elf32_word p_type;
	elf32_off p_offset;
	elf32_addr p_vaddr;
	elf32_addr p_paddr;
	elf32_word p_filesz;
	elf32_word p_memsz;
	elf32_word p_flags;
	elf32_word p_align;
};

/*
 * Segment types stored in the image headers.
 */
#define PT_NULL 0
#define PT_LOAD 1
#define PT_DYNAMIC 2
#define PT_INTERP 3
#define PT_NOTE 4
#define PT_SHLIB 5
#define PT_PHDR 6
#define PT_TLS 7

/*
 * ELF section header.
 */
struct elf32_shdr {
	elf32_word sh_name;
	elf32_word sh_type;
	elf32_word sh_flags;
	elf32_addr sh_addr;
	elf32_off sh_offset;
	elf32_word sh_size;
	elf32_word sh_link;
	elf32_word sh_info;
	elf32_word sh_addralign;
	elf32_word sh_entsize;
};

/*
 * Section indexes.
 */
#define SHN_UNDEF 0
#define SHN_LORESERVE 0xff00
#define SHN_LOPROC 0xff00
#define SHN_HIPROC 0xff1f
#define SHN_ABS 0xfff1
#define SHN_COMMON 0xfff2
#define SHN_HIRESERVE 0xffff

/*
 * Section types.
 */
#define SHT_NULL 0
#define SHT_PROGBITS 1
#define SHT_SYMTAB 2
#define SHT_STRTAB 3
#define SHT_RELA 4
#define SHT_HASH 5
#define SHT_DYNAMIC 6
#define SHT_NOTE 7
#define SHT_NOBITS 8
#define SHT_REL 9
#define SHT_SHLIB 10
#define SHT_DYNSYM 11
#define SHT_LOPROC 0x70000000
#define SHT_HIPROC 0x7fffffff
#define SHT_LOUSER 0x80000000
#define SHT_HIUSER 0xffffffff

/*
 * Section flags.
 */
#define SHF_WRITE 0x1
#define SHF_ALLOC 0x2
#define SHF_EXECINSTR 0x4

/*
 * Symbol table entry.
 */
struct elf32_sym {
	elf32_word st_name;
	elf32_addr st_value;
	elf32_word st_size;
	unsigned char st_info;
	unsigned char st_other;
	elf32_half st_shndx;
};

/*
 * Ubicom32 opcodes.
 */
enum opcodes {
	OP_ADD_1,
	OP_ADD_2,
	OP_ADD_4,
	OP_ADDC,
	OP_AND_1,
	OP_AND_2,
	OP_AND_4,
	OP_ASR_1,
	OP_ASR_2,
	OP_ASR_4,
	OP_BCLR,
	OP_BFEXTU,
	OP_BFRVRS,
	OP_BKPT,
	OP_BSET,
	OP_BTST,
	OP_CALL,
	OP_CALLI,
	OP_CMPI,
	OP_CRCGEN,
	OP_EXT_1,
	OP_EXT_2,
	OP_FLUSH,
	OP_IREAD,
	OP_IWRITE,
	OP_JMPF_S_F,
	OP_JMPLO_S_F,
	OP_JMPHS_S_F,
	OP_JMPEQ_S_F,
	OP_JMPGE_S_F,
	OP_JMPGT_S_F,
	OP_JMPHI_S_F,
	OP_JMPLE_S_F,
	OP_JMPLS_S_F,
	OP_JMPLT_S_F,
	OP_JMPMI_S_F,
	OP_JMPNE_S_F,
	OP_JMPPL_S_F,
	OP_JMPT_S_F,
	OP_JMPVC_S_F,
	OP_JMPVS_S_F,
	OP_JMPF_S_T,
	OP_JMPLO_S_T,
	OP_JMPHS_S_T,
	OP_JMPEQ_S_T,
	OP_JMPGE_S_T,
	OP_JMPGT_S_T,
	OP_JMPHI_S_T,
	OP_JMPLE_S_T,
	OP_JMPLS_S_T,
	OP_JMPLT_S_T,
	OP_JMPMI_S_T,
	OP_JMPNE_S_T,
	OP_JMPPL_S_T,
	OP_JMPT_S_T,
	OP_JMPVC_S_T,
	OP_JMPVS_S_T,
	OP_JMPF_W_F,
	OP_JMPLO_W_F,
	OP_JMPHS_W_F,
	OP_JMPEQ_W_F,
	OP_JMPGE_W_F,
	OP_JMPGT_W_F,
	OP_JMPHI_W_F,
	OP_JMPLE_W_F,
	OP_JMPLS_W_F,
	OP_JMPLT_W_F,
	OP_JMPMI_W_F,
	OP_JMPNE_W_F,
	OP_JMPPL_W_F,
	OP_JMPT_W_F,
	OP_JMPVC_W_F,
	OP_JMPVS_W_F,
	OP_JMPF_W_T,
	OP_JMPLO_W_T,
	OP_JMPHS_W_T,
	OP_JMPEQ_W_T,
	OP_JMPGE_W_T,
	OP_JMPGT_W_T,
	OP_JMPHI_W_T,
	OP_JMPLE_W_T,
	OP_JMPLS_W_T,
	OP_JMPLT_W_T,
	OP_JMPMI_W_T,
	OP_JMPNE_W_T,
	OP_JMPPL_W_T,
	OP_JMPT_W_T,
	OP_JMPVC_W_T,
	OP_JMPVS_W_T,
	OP_LEA_1,
	OP_LEA_2,
	OP_LEA_4,
	OP_LSL_1,
	OP_LSL_2,
	OP_LSL_4,
	OP_LSR_1,
	OP_LSR_2,
	OP_LSR_4,
	OP_MAC,
	OP_MADD_2,
	OP_MADD_2_C,
	OP_MADD_2_T,
	OP_MADD_2_C_T,
	OP_MADD_4,
	OP_MADD_4_C,
	OP_MADD_4_T,
	OP_MADD_4_C_T,
	OP_MACF,
	OP_MACF_C,
	OP_MACF_T,
	OP_MACF_C_T,
	OP_MACS,
	OP_MACS_C,
	OP_MACS_T,
	OP_MACS_C_T,
	OP_MACU,
	OP_MACU_C,
	OP_MACU_T,
	OP_MACU_C_T,
	OP_MACUS,
	OP_MACUS_C,
	OP_MACUS_T,
	OP_MACUS_C_T,
	OP_MERGE,
	OP_MOVE_1,
	OP_MOVE_2,
	OP_MOVE_4,
	OP_MOVEA,
	OP_MOVEAI,
	OP_MOVEI,
	OP_MSUB_2,
	OP_MSUB_2_C,
	OP_MSUB_2_T,
	OP_MSUB_2_C_T,
	OP_MSUB_4,
	OP_MSUB_4_C,
	OP_MSUB_4_T,
	OP_MSUB_4_C_T,
	OP_MSUF,
	OP_MSUF_C,
	OP_MSUF_T,
	OP_MSUF_C_T,
	OP_MULF,
	OP_MULF_C,
	OP_MULF_T,
	OP_MULF_C_T,
	OP_MULS,
	OP_MULS_T,
	OP_MULS_4,
	OP_MULU,
	OP_MULU_T,
	OP_MULU_4,
	OP_NOT_2,
	OP_NOT_4,
	OP_OR_1,
	OP_OR_2,
	OP_OR_4,
	OP_PDEC,
	OP_PREFETCH,
	OP_PXADDS,
	OP_PXADDS_U,
	OP_PXBLEND,
	OP_PXBLEND_T,
	OP_PXCNV,
	OP_PXCNV_T,
	OP_PXHI,
	OP_PXHI_S,
	OP_PXVI,
	OP_PXVI_S,
	OP_RET,
	OP_SETCSR,
	OP_SHFTD,
	OP_SHMRG_1,
	OP_SHMRG_2,
	OP_SUB_1,
	OP_SUB_2,
	OP_SUB_4,
	OP_SUBC,
	OP_SUSPEND,
	OP_SWAPB_2,
	OP_SWAPB_4,
	OP_SYNC,
	OP_SYSCALL,
	OP_SYSRET,
	OP_XOR_1,
	OP_XOR_2,
	OP_XOR_4,
	OP_UNUSED
};

struct opcode {
	const char name[10];			/* Opcode name */
	int size;				/* Size of the instruction operation */
	int isa_bitmap;				/* Bitmap of ISA versions in which supported */
};

#define ISA_1 (1 << 0)
#define ISA_2 (1 << 1)
#define ISA_3 (1 << 2)
#define ISA_4 (1 << 3)
#define ISA_5 (1 << 4)

struct opcode opcodes[OP_UNUSED + 1] = {
	{
		"add.1",
		1,
		ISA_4 | ISA_5
	},
	{
		"add.2",
		2,
		ISA_1 | ISA_2 | ISA_3 | ISA_4 | ISA_5
	},
	{
		"add.4",
		4,
		ISA_1 | ISA_2 | ISA_3 | ISA_4 | ISA_5
	},
	{
		"addc",
		4,
		ISA_1 | ISA_2 | ISA_3 | ISA_4 | ISA_5
	},
	{
		"and.1",
		1,
		ISA_4 | ISA_5
	},
	{
		"and.2",
		2,
		ISA_1 | ISA_2 | ISA_3 | ISA_4 | ISA_5
	},
	{
		"and.4",
		4,
		ISA_1 | ISA_2 | ISA_3 | ISA_4 | ISA_5
	},
	{
		"asr.1",
		1,
		ISA_4 | ISA_5
	},
	{
		"asr.2",
		2,
		ISA_1 | ISA_2 | ISA_3 | ISA_4 | ISA_5
	},
	{
		"asr.4",
		4,
		ISA_1 | ISA_2 | ISA_3 | ISA_4 | ISA_5
	},
	{
		"bclr",
		4,
		ISA_1 | ISA_2 | ISA_3 | ISA_4 | ISA_5
	},
	{
		"bfextu",
		4,
		ISA_1 | ISA_2 | ISA_3 | ISA_4 | ISA_5
	},
	{
		"bfrvrs",
		4,
		ISA_1 | ISA_2 | ISA_3 | ISA_4 | ISA_5
	},
	{
		"bkpt",
		0,
		ISA_1 | ISA_2 | ISA_3 | ISA_4 | ISA_5
	},
	{
		"bset",
		4,
		ISA_1 | ISA_2 | ISA_3 | ISA_4 | ISA_5
	},
	{
		"btst",
		4,
		ISA_1 | ISA_2 | ISA_3 | ISA_4 | ISA_5
	},
	{
		"call",
		0,
		ISA_1 | ISA_2 | ISA_3 | ISA_4 | ISA_5
	},
	{
		"calli",
		4,
		ISA_1 | ISA_2 | ISA_3 | ISA_4 | ISA_5
	},
	{
		"cmpi",
		2,
		ISA_1 | ISA_2 | ISA_3 | ISA_4 | ISA_5
	},
	{
		"crcgen",
		4,
		ISA_1 | ISA_2 | ISA_3 | ISA_4 | ISA_5
	},
	{
		"ext.1",
		1,
		ISA_1 | ISA_2 | ISA_3 | ISA_4 | ISA_5
	},
	{
		"ext.2",
		2,
		ISA_1 | ISA_2 | ISA_3 | ISA_4 | ISA_5
	},
	{
		"flush",
		4,
		ISA_5
	},
	{
		"iread",
		4,
		ISA_1 | ISA_2
	},
	{
		"iwrite",
		4,
		ISA_1 | ISA_2
	},
	{
		"jmpf.s.f",
		0,
		ISA_1 | ISA_2 | ISA_3 | ISA_4 | ISA_5
	},
	{
		"jmplo.s.f",
		0,
		ISA_1 | ISA_2 | ISA_3 | ISA_4 | ISA_5
	},
	{
		"jmphs.s.f",
		0,
		ISA_1 | ISA_2 | ISA_3 | ISA_4 | ISA_5
	},
	{
		"jmpeq.s.f",
		0,
		ISA_1 | ISA_2 | ISA_3 | ISA_4 | ISA_5
	},
	{
		"jmpge.s.f",
		0,
		ISA_1 | ISA_2 | ISA_3 | ISA_4 | ISA_5
	},
	{
		"jmpgt.s.f",
		0,
		ISA_1 | ISA_2 | ISA_3 | ISA_4 | ISA_5
	},
	{
		"jmphi.s.f",
		0,
		ISA_1 | ISA_2 | ISA_3 | ISA_4 | ISA_5
	},
	{
		"jmple.s.f",
		0,
		ISA_1 | ISA_2 | ISA_3 | ISA_4 | ISA_5
	},
	{
		"jmpls.s.f",
		0,
		ISA_1 | ISA_2 | ISA_3 | ISA_4 | ISA_5
	},
	{
		"jmplt.s.f",
		0,
		ISA_1 | ISA_2 | ISA_3 | ISA_4 | ISA_5
	},
	{
		"jmpmi.s.f",
		0,
		ISA_1 | ISA_2 | ISA_3 | ISA_4 | ISA_5
	},
	{
		"jmpne.s.f",
		0,
		ISA_1 | ISA_2 | ISA_3 | ISA_4 | ISA_5
	},
	{
		"jmppl.s.f",
		0,
		ISA_1 | ISA_2 | ISA_3 | ISA_4 | ISA_5
	},
	{
		"jmpt.s.f",
		0,
		ISA_1 | ISA_2 | ISA_3 | ISA_4 | ISA_5
	},
	{
		"jmpvc.s.f",
		0,
		ISA_1 | ISA_2 | ISA_3 | ISA_4 | ISA_5
	},
	{
		"jmpvs.s.f",
		0,
		ISA_1 | ISA_2 | ISA_3 | ISA_4 | ISA_5
	},
	{
		"jmpf.s.t",
		0,
		ISA_1 | ISA_2 | ISA_3 | ISA_4 | ISA_5
	},
	{
		"jmplo.s.t",
		0,
		ISA_1 | ISA_2 | ISA_3 | ISA_4 | ISA_5
	},
	{
		"jmphs.s.t",
		0,
		ISA_1 | ISA_2 | ISA_3 | ISA_4 | ISA_5
	},
	{
		"jmpeq.s.t",
		0,
		ISA_1 | ISA_2 | ISA_3 | ISA_4 | ISA_5
	},
	{
		"jmpge.s.t",
		0,
		ISA_1 | ISA_2 | ISA_3 | ISA_4 | ISA_5
	},
	{
		"jmpgt.s.t",
		0,
		ISA_1 | ISA_2 | ISA_3 | ISA_4 | ISA_5
	},
	{
		"jmphi.s.t",
		0,
		ISA_1 | ISA_2 | ISA_3 | ISA_4 | ISA_5
	},
	{
		"jmple.s.t",
		0,
		ISA_1 | ISA_2 | ISA_3 | ISA_4 | ISA_5
	},
	{
		"jmpls.s.t",
		0,
		ISA_1 | ISA_2 | ISA_3 | ISA_4 | ISA_5
	},
	{
		"jmplt.s.t",
		0,
		ISA_1 | ISA_2 | ISA_3 | ISA_4 | ISA_5
	},
	{
		"jmpmi.s.t",
		0,
		ISA_1 | ISA_2 | ISA_3 | ISA_4 | ISA_5
	},
	{
		"jmpne.s.t",
		0,
		ISA_1 | ISA_2 | ISA_3 | ISA_4 | ISA_5
	},
	{
		"jmppl.s.t",
		0,
		ISA_1 | ISA_2 | ISA_3 | ISA_4 | ISA_5
	},
	{
		"jmpt.s.t",
		0,
		ISA_1 | ISA_2 | ISA_3 | ISA_4 | ISA_5
	},
	{
		"jmpvc.s.t",
		0,
		ISA_1 | ISA_2 | ISA_3 | ISA_4 | ISA_5
	},
	{
		"jmpvs.s.t",
		0,
		ISA_1 | ISA_2 | ISA_3 | ISA_4 | ISA_5
	},
	{
		"jmpf.w.f",
		0,
		ISA_1 | ISA_2 | ISA_3 | ISA_4 | ISA_5
	},
	{
		"jmplo.w.f",
		0,
		ISA_1 | ISA_2 | ISA_3 | ISA_4 | ISA_5
	},
	{
		"jmphs.w.f",
		0,
		ISA_1 | ISA_2 | ISA_3 | ISA_4 | ISA_5
	},
	{
		"jmpeq.w.f",
		0,
		ISA_1 | ISA_2 | ISA_3 | ISA_4 | ISA_5
	},
	{
		"jmpge.w.f",
		0,
		ISA_1 | ISA_2 | ISA_3 | ISA_4 | ISA_5
	},
	{
		"jmpgt.w.f",
		0,
		ISA_1 | ISA_2 | ISA_3 | ISA_4 | ISA_5
	},
	{
		"jmphi.w.f",
		0,
		ISA_1 | ISA_2 | ISA_3 | ISA_4 | ISA_5
	},
	{
		"jmple.w.f",
		0,
		ISA_1 | ISA_2 | ISA_3 | ISA_4 | ISA_5
	},
	{
		"jmpls.w.f",
		0,
		ISA_1 | ISA_2 | ISA_3 | ISA_4 | ISA_5
	},
	{
		"jmplt.w.f",
		0,
		ISA_1 | ISA_2 | ISA_3 | ISA_4 | ISA_5
	},
	{
		"jmpmi.w.f",
		0,
		ISA_1 | ISA_2 | ISA_3 | ISA_4 | ISA_5
	},
	{
		"jmpne.w.f",
		0,
		ISA_1 | ISA_2 | ISA_3 | ISA_4 | ISA_5
	},
	{
		"jmppl.w.f",
		0,
		ISA_1 | ISA_2 | ISA_3 | ISA_4 | ISA_5
	},
	{
		"jmpt.w.f",
		0,
		ISA_1 | ISA_2 | ISA_3 | ISA_4 | ISA_5
	},
	{
		"jmpvc.w.f",
		0,
		ISA_1 | ISA_2 | ISA_3 | ISA_4 | ISA_5
	},
	{
		"jmpvs.w.f",
		0,
		ISA_1 | ISA_2 | ISA_3 | ISA_4 | ISA_5
	},
	{
		"jmpf.w.t",
		0,
		ISA_1 | ISA_2 | ISA_3 | ISA_4 | ISA_5
	},
	{
		"jmplo.w.t",
		0,
		ISA_1 | ISA_2 | ISA_3 | ISA_4 | ISA_5
	},
	{
		"jmphs.w.t",
		0,
		ISA_1 | ISA_2 | ISA_3 | ISA_4 | ISA_5
	},
	{
		"jmpeq.w.t",
		0,
		ISA_1 | ISA_2 | ISA_3 | ISA_4 | ISA_5
	},
	{
		"jmpge.w.t",
		0,
		ISA_1 | ISA_2 | ISA_3 | ISA_4 | ISA_5
	},
	{
		"jmpgt.w.t",
		0,
		ISA_1 | ISA_2 | ISA_3 | ISA_4 | ISA_5
	},
	{
		"jmphi.w.t",
		0,
		ISA_1 | ISA_2 | ISA_3 | ISA_4 | ISA_5
	},
	{
		"jmple.w.t",
		0,
		ISA_1 | ISA_2 | ISA_3 | ISA_4 | ISA_5
	},
	{
		"jmpls.w.t",
		0,
		ISA_1 | ISA_2 | ISA_3 | ISA_4 | ISA_5
	},
	{
		"jmplt.w.t",
		0,
		ISA_1 | ISA_2 | ISA_3 | ISA_4 | ISA_5
	},
	{
		"jmpmi.w.t",
		0,
		ISA_1 | ISA_2 | ISA_3 | ISA_4 | ISA_5
	},
	{
		"jmpne.w.t",
		0,
		ISA_1 | ISA_2 | ISA_3 | ISA_4 | ISA_5
	},
	{
		"jmppl.w.t",
		0,
		ISA_1 | ISA_2 | ISA_3 | ISA_4 | ISA_5
	},
	{
		"jmpt.w.t",
		0,
		ISA_1 | ISA_2 | ISA_3 | ISA_4 | ISA_5
	},
	{
		"jmpvc.w.t",
		0,
		ISA_1 | ISA_2 | ISA_3 | ISA_4 | ISA_5
	},
	{
		"jmpvs.w.t",
		0,
		ISA_1 | ISA_2 | ISA_3 | ISA_4 | ISA_5
	},
	{
		"lea.1",
		1,
		ISA_1 | ISA_2 | ISA_3 | ISA_4 | ISA_5
	},
	{
		"lea.2",
		2,
		ISA_1 | ISA_2 | ISA_3 | ISA_4 | ISA_5
	},
	{
		"lea.4",
		4,
		ISA_1 | ISA_2 | ISA_3 | ISA_4 | ISA_5
	},
	{
		"lsl.1",
		1,
		ISA_4 | ISA_5
	},
	{
		"lsl.2",
		2,
		ISA_1 | ISA_2 | ISA_3 | ISA_4 | ISA_5
	},
	{
		"lsl.4",
		4,
		ISA_1 | ISA_2 | ISA_3 | ISA_4 | ISA_5
	},
	{
		"lsr.1",
		1,
		ISA_4 | ISA_5
	},
	{
		"lsr.2",
		2,
		ISA_1 | ISA_2 | ISA_3 | ISA_4 | ISA_5
	},
	{
		"lsr.4",
		4,
		ISA_1 | ISA_2 | ISA_3 | ISA_4 | ISA_5
	},
	{
		"mac",
		2,
		ISA_1 | ISA_2 | ISA_3 | ISA_4 | ISA_5
	},
	{
		"madd.2",
		2,
		ISA_3 | ISA_4 | ISA_5
	},
	{
		"madd.2.c",
		2,
		ISA_3 | ISA_4 | ISA_5
	},
	{
		"madd.2.t",
		2,
		ISA_3 | ISA_4 | ISA_5
	},
	{
		"madd.2.c.t",
		2,
		ISA_3 | ISA_4 | ISA_5
	},
	{
		"madd.4",
		4,
		ISA_3 | ISA_4 | ISA_5
	},
	{
		"madd.4.c",
		4,
		ISA_3 | ISA_4 | ISA_5
	},
	{
		"madd.4.t",
		4,
		ISA_3 | ISA_4 | ISA_5
	},
	{
		"madd.4.c.t",
		4,
		ISA_3 | ISA_4 | ISA_5
	},
	{
		"macf",
		2,
		ISA_3 | ISA_4 | ISA_5
	},
	{
		"macf.c",
		2,
		ISA_3 | ISA_4 | ISA_5
	},
	{
		"macf.t",
		2,
		ISA_3 | ISA_4 | ISA_5
	},
	{
		"macf.c.t",
		2,
		ISA_3 | ISA_4 | ISA_5
	},
	{
		"macs",
		2,
		ISA_3 | ISA_4 | ISA_5
	},
	{
		"macs.c",
		2,
		ISA_3 | ISA_4 | ISA_5
	},
	{
		"macs.t",
		2,
		ISA_3 | ISA_4 | ISA_5
	},
	{
		"macs.c.t",
		2,
		ISA_3 | ISA_4 | ISA_5
	},
	{
		"macu",
		2,
		ISA_3 | ISA_4 | ISA_5
	},
	{
		"macu.c",
		2,
		ISA_3 | ISA_4 | ISA_5
	},
	{
		"macu.t",
		2,
		ISA_3 | ISA_4 | ISA_5
	},
	{
		"macu.c.t",
		2,
		ISA_3 | ISA_4 | ISA_5
	},
	{
		"macus",
		2,
		ISA_3 | ISA_4 | ISA_5
	},
	{
		"macus.c",
		2,
		ISA_3 | ISA_4 | ISA_5
	},
	{
		"macus.t",
		2,
		ISA_3 | ISA_4 | ISA_5
	},
	{
		"macus.c.t",
		2,
		ISA_3 | ISA_4 | ISA_5
	},
	{
		"merge",
		4,
		ISA_1 | ISA_2 | ISA_3 | ISA_4 | ISA_5
	},
	{
		"move.1",
		1,
		ISA_1 | ISA_2 | ISA_3 | ISA_4 | ISA_5
	},
	{
		"move.2",
		2,
		ISA_1 | ISA_2 | ISA_3 | ISA_4 | ISA_5
	},
	{
		"move.4",
		4,
		ISA_1 | ISA_2 | ISA_3 | ISA_4 | ISA_5
	},
	{
		"movea",
		4,
		ISA_3 | ISA_4 | ISA_5
	},
	{
		"moveai",
		0,
		ISA_1 | ISA_2 | ISA_3 | ISA_4 | ISA_5
	},
	{
		"movei",
		2,
		ISA_1 | ISA_2 | ISA_3 | ISA_4 | ISA_5
	},
	{
		"msub.2",
		2,
		ISA_3 | ISA_4 | ISA_5
	},
	{
		"msub.2.c",
		2,
		ISA_3 | ISA_4 | ISA_5
	},
	{
		"msub.2.t",
		2,
		ISA_3 | ISA_4 | ISA_5
	},
	{
		"msub.2.c.t",
		2,
		ISA_3 | ISA_4 | ISA_5
	},
	{
		"msub.4",
		4,
		ISA_3 | ISA_4 | ISA_5
	},
	{
		"msub.4.c",
		4,
		ISA_3 | ISA_4 | ISA_5
	},
	{
		"msub.4.t",
		4,
		ISA_3 | ISA_4 | ISA_5
	},
	{
		"msub.4.c.t",
		4,
		ISA_3 | ISA_4 | ISA_5
	},
	{
		"msuf",
		2,
		ISA_3 | ISA_4 | ISA_5
	},
	{
		"msuf.c",
		2,
		ISA_3 | ISA_4 | ISA_5
	},
	{
		"msuf.t",
		2,
		ISA_3 | ISA_4 | ISA_5
	},
	{
		"msuf.c.t",
		2,
		ISA_3 | ISA_4 | ISA_5
	},
	{
		"mulf",
		2,
		ISA_1 | ISA_2 | ISA_3 | ISA_4 | ISA_5
	},
	{
		"mulf.c",
		2,
		ISA_3 | ISA_4 | ISA_5
	},
	{
		"mulf.t",
		2,
		ISA_3 | ISA_4 | ISA_5
	},
	{
		"mulf.c.t",
		2,
		ISA_3 | ISA_4 | ISA_5
	},
	{
		"muls",
		2,
		ISA_1 | ISA_2 | ISA_3 | ISA_4 | ISA_5
	},
	{
		"muls.t",
		2,
		ISA_3 | ISA_4 | ISA_5
	},
	{
		"muls.4",
		4,
		ISA_4 | ISA_5
	},
	{
		"mulu",
		2,
		ISA_1 | ISA_2 | ISA_3 | ISA_4 | ISA_5
	},
	{
		"mulu.t",
		2,
		ISA_3 | ISA_4 | ISA_5
	},
	{
		"mulu.4",
		4,
		ISA_4 | ISA_5
	},
	{
		"not.2",
		2,
		ISA_1 | ISA_2 | ISA_3 | ISA_4 | ISA_5
	},
	{
		"not.4",
		4,
		ISA_1 | ISA_2 | ISA_3 | ISA_4 | ISA_5
	},
	{
		"or.1",
		1,
		ISA_4 | ISA_5
	},
	{
		"or.2",
		2,
		ISA_1 | ISA_2 | ISA_3 | ISA_4 | ISA_5
	},
	{
		"or.4",
		4,
		ISA_1 | ISA_2 | ISA_3 | ISA_4 | ISA_5
	},
	{
		"pdec",
		4,
		ISA_1 | ISA_2 | ISA_3 | ISA_4 | ISA_5
	},
	{
		"prefetch",
		4,
		ISA_5
	},
	{
		"pxadds",
		4,
		ISA_4 | ISA_5
	},
	{
		"pxadds.u",
		4,
		ISA_4 | ISA_5
	},
	{
		"pxblend",
		4,
		ISA_4 | ISA_5
	},
	{
		"pxblend.t",
		4,
		ISA_4 | ISA_5
	},
	{
		"pxcnv",
		4,
		ISA_4 | ISA_5
	},
	{
		"pxcnv.t",
		4,
		ISA_4 | ISA_5
	},
	{
		"pxhi",
		4,
		ISA_4 | ISA_5
	},
	{
		"pxhi.s",
		4,
		ISA_4 | ISA_5
	},
	{
		"pxvi",
		4,
		ISA_4 | ISA_5
	},
	{
		"pxvi.s",
		4,
		ISA_4 | ISA_5
	},
	{
		"ret",
		4,
		ISA_1 | ISA_2 | ISA_3 | ISA_4 | ISA_5
	},
	{
		"setcsr",
		4,
		ISA_1 | ISA_2 | ISA_3 | ISA_4 | ISA_5
	},
	{
		"shiftd",
		4,
		ISA_1 | ISA_2 | ISA_3 | ISA_4 | ISA_5
	},
	{
		"shmrg.1",
		1,
		ISA_1 | ISA_2 | ISA_3 | ISA_4 | ISA_5
	},
	{
		"shmrg.2",
		2,
		ISA_1 | ISA_2 | ISA_3 | ISA_4 | ISA_5
	},
	{
		"sub.1",
		1,
		ISA_4 | ISA_5
	},
	{
		"sub.2",
		2,
		ISA_1 | ISA_2 | ISA_3 | ISA_4 | ISA_5
	},
	{
		"sub.4",
		4,
		ISA_1 | ISA_2 | ISA_3 | ISA_4 | ISA_5
	},
	{
		"subc",
		4,
		ISA_1 | ISA_2 | ISA_3 | ISA_4 | ISA_5
	},
	{
		"suspend",
		0,
		ISA_1 | ISA_2 | ISA_3 | ISA_4 | ISA_5
	},
	{
		"swapb.2",
		2,
		ISA_4 | ISA_5
	},
	{
		"swapb.4",
		4,
		ISA_4 | ISA_5
	},
	{
		"sync",
		4,
		ISA_5
	},
	{
		"syscall",
		4,
		ISA_5
	},
	{
		"sysret",
		4,
		ISA_5
	},
	{
		"xor.1",
		1,
		ISA_4 | ISA_5
	},
	{
		"xor.2",
		2,
		ISA_1 | ISA_2 | ISA_3 | ISA_4 | ISA_5
	},
	{
		"xor.4",
		4,
		ISA_1 | ISA_2 | ISA_3 | ISA_4 | ISA_5
	},
	{
		"unused",
		0,
		ISA_1 | ISA_2 | ISA_3 | ISA_4 | ISA_5
	}
};

/*
 * Register names.
 */
const char regname[0x102][17] = {
	"d0",
	"d1",
	"d2",
	"d3",
	"d4",
	"d5",
	"d6",
	"d7",
	"d8",
	"d9",
	"d10",
	"d11",
	"d12",
	"d13",
	"d14",
	"d15",
	"reserved_10",
	"reserved_11",
	"reserved_12",
	"reserved_13",
	"reserved_14",
	"reserved_15",
	"reserved_16",
	"reserved_17",
	"reserved_18",
	"reserved_19",
	"reserved_1a",
	"reserved_1b",
	"reserved_1c",
	"reserved_1d",
	"reserved_1e",
	"reserved_1f",
	"a0",
	"a1",
	"a2",
	"a3",
	"a4",
	"a5",
	"a6",
	"sp",
	"acc0_hi",
	"acc0_lo",
	"mac_rc16",
	"source3",
	"inst_cnt",
	"csr",
	"rosr",
	"iread_data",
	"int_mask0",
	"int_mask1",
	"reserved_32",
	"reserved_33",
	"pc",
	"trap_cause",
	"acc1_hi",
	"acc1_lo",
	"previous_pc",
	"pcsr",
	"reserved_3a",
	"reserved_3b",
	"reserved_3c",
	"reserved_3d",
	"reserved_3e",
	"reserved_3f",
	"chip_id",
	"int_stat0",
	"int_stat1",
	"reserved_43",
	"reserved_44",
	"int_set0",
	"int_set1",
	"reserved_47",
	"reserved_48",
	"int_clr0",
	"int_clr1",
	"reserved_4b",
	"reserved_4c",
	"global_ctrl",
	"mt_active",
	"mt_active_set",
	"mt_active_clr",
	"mt_dbg_active",
	"mt_dbg_active_set",
	"mt_en",
	"mt_hpri",
	"mt_hrt",
	"mt_break",
	"mt_break_clr",
	"mt_single_step",
	"mt_min_delay_en",
	"mt_break_set",
	"perr_addr",
	"dcapt",
	"dcapt_pc",
	"dcapt_tnum",
	"mt_dbg_active_clr",
	"scratchpad0",
	"scratchpad1",
	"scratchpad2",
	"scratchpad3",
	"reserved_64",
	"reserved_65",
	"reserved_66",
	"reserved_67",
	"cfg",
	"reserved_69",
	"reserved_6a",
	"reserved_6b",
	"reserved_6c",
	"reserved_6d",
	"mt_trap_en",
	"mt_trap",
	"mt_trap_set",
	"mt_trap_clr",
	"sep",
	"reserved_73",
	"reserved_74",
	"reserved_75",
	"reserved_76",
	"reserved_77",
	"reserved_78",
	"reserved_79",
	"reserved_7a",
	"reserved_7b",
	"reserved_7c",
	"reserved_7d",
	"reserved_7e",
	"reserved_7f",
	"reserved_80",
	"reserved_81",
	"reserved_82",
	"reserved_83",
	"reserved_84",
	"reserved_85",
	"reserved_86",
	"reserved_87",
	"reserved_88",
	"reserved_89",
	"reserved_8a",
	"reserved_8b",
	"reserved_8c",
	"reserved_8d",
	"reserved_8e",
	"reserved_8f",
	"reserved_90",
	"reserved_91",
	"reserved_92",
	"reserved_93",
	"reserved_94",
	"reserved_95",
	"reserved_96",
	"reserved_97",
	"reserved_98",
	"reserved_99",
	"reserved_9a",
	"reserved_9b",
	"reserved_9c",
	"reserved_9d",
	"reserved_9e",
	"reserved_9f",
	"reserved_a0",
	"reserved_a1",
	"reserved_a2",
	"reserved_a3",
	"reserved_a4",
	"reserved_a5",
	"reserved_a6",
	"reserved_a7",
	"reserved_a8",
	"reserved_a9",
	"reserved_aa",
	"reserved_ab",
	"reserved_ac",
	"reserved_ad",
	"reserved_ae",
	"reserved_af",
	"reserved_b0",
	"reserved_b1",
	"reserved_b2",
	"reserved_b3",
	"reserved_b4",
	"reserved_b5",
	"reserved_b6",
	"reserved_b7",
	"reserved_b8",
	"reserved_b9",
	"reserved_ba",
	"reserved_bb",
	"reserved_bc",
	"reserved_bd",
	"reserved_be",
	"reserved_bf",
	"reserved_c0",
	"reserved_c1",
	"reserved_c2",
	"reserved_c3",
	"reserved_c4",
	"reserved_c5",
	"reserved_c6",
	"reserved_c7",
	"reserved_c8",
	"reserved_c9",
	"reserved_ca",
	"reserved_cb",
	"reserved_cc",
	"reserved_cd",
	"reserved_ce",
	"reserved_cf",
	"reserved_d0",
	"reserved_d1",
	"reserved_d2",
	"reserved_d3",
	"reserved_d4",
	"reserved_d5",
	"reserved_d6",
	"reserved_d7",
	"reserved_d8",
	"reserved_d9",
	"reserved_da",
	"reserved_db",
	"reserved_dc",
	"reserved_dd",
	"reserved_de",
	"reserved_df",
	"reserved_e0",
	"reserved_e1",
	"reserved_e2",
	"reserved_e3",
	"reserved_e4",
	"reserved_e5",
	"reserved_e6",
	"reserved_e7",
	"reserved_e8",
	"reserved_e9",
	"reserved_ea",
	"reserved_eb",
	"reserved_ec",
	"reserved_ed",
	"reserved_ee",
	"reserved_ef",
	"reserved_f0",
	"reserved_f1",
	"reserved_f2",
	"reserved_f3",
	"reserved_f4",
	"reserved_f5",
	"reserved_f6",
	"reserved_f7",
	"reserved_f8",
	"reserved_f9",
	"reserved_fa",
	"reserved_fb",
	"reserved_fc",
	"reserved_fd",
	"reserved_fe",
	"reserved_ff",
	"acc0",
	"acc1"
};

/*
 * Element types.
 */
enum element_type {
	ET_OPCODE,
	ET_LABEL,
	ET_BLOCK_SEPARATOR,
	ET_SECTION,
	ET_SYMBOL
};

/*
 * code element.
 *	Base structure type used for code sequences.
 *
 * This is effectively a base class from which can be derived a number
 * of concrete classes.  The key aspect of this base class is that is
 * allows efficient representation of the derived classes and allows
 * the various subclasses to be linked together into a single list.
 */
struct code_element {
	enum element_type type;			/* What type of element is this? */
	struct code_element *prev;		/* Previous code element */
	struct code_element *next;		/* Next code element */
};

/*
 * Label element.
 *	This represents a single label annotation.
 */
struct code_element_label {
	struct code_element ce;			/* Base class - must be first in the structure */
	uint32_t addr;				/* Address */
	int refs;				/* Number of references to this label */
	int number;				/* Label number */
	bool call_label;			/* Is this a call target? */
	struct code_element_label *hash_next;	/* Next label in hash */
	char *name;				/* Name associated with this label */
};

/*
 * Block separator element.
 *	This represents a single basic block separator annotation.
 */
struct code_element_block_separator {
	struct code_element ce;			/* Base class - must be first in the structure */
};

/*
 * Section element.
 *	This represents the start of a section.
 */
struct code_element_section {
	struct code_element ce;			/* Base class - must be first in the structure */
	char *name;				/* Section name */
	int number;				/* Section number */
	struct code_element_section *hash_next;	/* Next section in hash */
};

/*
 * Symbol element.
 *	This represents a single symbol table annotation.
 */
struct code_element_symbol {
	struct code_element ce;			/* Base class - must be first in the structure */
	uint32_t addr;				/* Address */
	uint32_t str_offs;			/* Offset into strings section for the name */
	char *name;				/* Symbol name */
	int type;				/* Symbol type */
	int section;				/* Symbol section */
};

/*
 * Operand modes.
 */
enum operand_mode {
	OM_AREG_PLUS_IMM,
	OM_AREG_PLUS_DREG,
	OM_AREG_MODIFY,
	OM_REG,
	OM_IMM,
	OM_UNUSED
};

/*
 * Operand encodings.
 */
struct operand_areg_plus_imm {
	int areg;
	int imm;
};

struct operand_areg_plus_dreg {
	int areg;
	int dreg;
};

struct operand_areg_modify {
	int areg;
	int modify;
};

struct operand_reg {
	int reg;
};

struct operand_imm {
	int imm;
};

/*
 * Operand.
 */
struct operand {
	enum operand_mode mode;
	union {
		struct operand_areg_plus_imm oapi;
		struct operand_areg_plus_dreg oapd;
		struct operand_areg_modify oam;
		struct operand_reg or;
		struct operand_imm oi;
	} value;
};

/*
 * Opcode element.
 *	This represents a single Dalvik opcode.
 */
struct code_element_opcode {
	struct code_element ce;			/* Base class - must be first in the structure */
	uint32_t instr;				/* Actual instruction word */
	uint32_t addr;				/* Address */
	enum opcodes opcode;			/* Opcode */
	struct operand src1;
	struct operand src2;
	struct operand dest;
	struct code_element_label *label;	/* Any label we reference */
};

#define LABEL_HASH_SLOTS 256			/* Number of slots in the label hash table */
#define SECTION_HASH_SLOTS 256			/* Number of slots in the section hash table */

struct elf32_hdr *elf_hdr;
struct code_element *code_head;
struct code_element *code_tail;
struct code_element_label *label_hash[LABEL_HASH_SLOTS];
struct code_element_section *section_hash[SECTION_HASH_SLOTS];
int opcode_counts[OP_UNUSED + 1];
int isa_version = 4;				// XXX - not really true - need to autodetect
int total_insns;
char *elf_shstrings;
char *elf_strings;
int symtab_section;				/* Symbol table section number */
int dynsym_section;				/* Dynamic symbol table section number */

/*
 * elf_uint16_to_host()
 *	Convert 16-bit value from the form in the ELF file to the form required for the host.
 */
uint16_t elf_uint16_to_host(uint16_t v)
{
	if (elf_hdr->e_ident[EI_DATA] == ELFDATA2MSB) {
		return htons(v);
	}

	return v;
}

/*
 * elf_uint32_to_host()
 *	Convert 32-bit value from the form in the ELF file to the form required for the host.
 */
uint32_t elf_uint32_to_host(uint32_t v)
{
	if (elf_hdr->e_ident[EI_DATA] == ELFDATA2MSB) {
		return htonl(v);
	}

	return v;
}

/*
 * decode_general_operand()
 */
static void decode_general_operand(struct operand *operand, uint32_t op)
{
	if (op & 0x400) {
		operand->mode = OM_AREG_PLUS_IMM;
		operand->value.oapi.areg = 0x20 + ((op >> 5) & 0x7);
		operand->value.oapi.imm = (op & 0x1f) | ((op & 0x300) >> 3);
		return;
	}

	switch ((op & 0x300) >> 8) {
	case 0:
		operand->mode = OM_IMM;
		operand->value.oi.imm = (int8_t)(op & 0xff);
		break;

	case 1:
		operand->mode = OM_REG;
		operand->value.or.reg = op & 0xff;
		break;

	case 2:
		operand->mode = OM_AREG_MODIFY;
		operand->value.oam.areg = 0x20 + ((op >> 5) & 0x7);
		operand->value.oam.modify = op & 0x1f;
		break;

	case 3:
		operand->mode = OM_AREG_PLUS_DREG;
		operand->value.oapd.areg = 0x20 + ((op >> 5) & 0x7);
		operand->value.oapd.dreg = op & 0xf;
	}
}

/*
 * decode_imm_operand()
 */
static void decode_imm_operand(struct operand *operand, int imm)
{
	operand->mode = OM_IMM;
	operand->value.oi.imm = imm;
}

/*
 * decode_dreg_operand()
 */
static void decode_reg_operand(struct operand *operand, int reg)
{
	operand->mode = OM_REG;
	operand->value.or.reg = reg;
}

/*
 * decode_insn_format_1()
 */
static void decode_insn_format_1(struct code_element_opcode *ceo, uint32_t op)
{
	decode_general_operand(&ceo->dest, (op >> 16) & 0x7ff);
	decode_general_operand(&ceo->src1, op & 0x7ff);

	int ext = (op >> 11) & 0x1f;
	switch (ext) {
	case 0x01:
		ceo->opcode = OP_SUSPEND;
		break;

	case 0x02:
		ceo->opcode = OP_FLUSH;
		break;

	case 0x03:
		ceo->opcode = OP_SYNC;
		break;

	case 0x04:
		ceo->opcode = OP_RET;
		break;

	case 0x05:
		ceo->opcode = OP_PREFETCH;
		break;

	case 0x06:
		ceo->opcode = OP_IREAD;
		break;

	case 0x07:
		ceo->opcode = OP_BKPT;
		break;

	case 0x08:
		ceo->opcode = OP_SYSRET;
		break;

	case 0x09:
		ceo->opcode = OP_SYSCALL;
		break;

	case 0x0a:
		ceo->opcode = OP_NOT_4;
		break;

	case 0x0b:
		ceo->opcode = OP_NOT_2;
		break;

	case 0x0c:
		ceo->opcode = OP_MOVE_4;
		break;

	case 0x0d:
		ceo->opcode = OP_MOVE_2;
		break;

	case 0x0e:
		ceo->opcode = OP_MOVEA;
		break;

	case 0x0f:
		ceo->opcode = OP_MOVE_1;
		break;

	case 0x10:
		ceo->opcode = OP_IWRITE;
		break;

	case 0x12:
		ceo->opcode = OP_SETCSR;
		break;

	case 0x15:
		ceo->opcode = OP_EXT_2;
		break;

	case 0x17:
		ceo->opcode = OP_EXT_1;
		break;

	case 0x18:
		ceo->opcode = OP_SWAPB_2;
		break;

	case 0x19:
		ceo->opcode = OP_SWAPB_4;
		break;

	case 0x1a:
		ceo->opcode = OP_PXCNV;
		break;

	case 0x1b:
		ceo->opcode = OP_PXCNV_T;
		break;

	case 0x1c:
		ceo->opcode = OP_LEA_4;
		break;

	case 0x1d:
		ceo->opcode = OP_LEA_2;
		break;

	case 0x1e:
		ceo->opcode = OP_PDEC;
		break;

	case 0x1f:
		ceo->opcode = OP_LEA_1;
		break;

	default:
		ceo->opcode = OP_UNUSED;
	}
}

/*
 * decode_insn_format_2()
 */
static void decode_insn_format_2(struct code_element_opcode *ceo, uint32_t op)
{
	decode_general_operand(&ceo->dest, (op >> 16) & 0x7ff);
	decode_general_operand(&ceo->src1, op & 0x7ff);
	decode_imm_operand(&ceo->src2, (op >> 11) & 0x1f);

	switch ((op >> 27) & 0x1f) {
	case 0x04:
		ceo->opcode = OP_BSET;
		break;

	case 0x05:
		ceo->opcode = OP_BCLR;
		break;

	default:
		ceo->opcode = OP_UNUSED;
	}
}

/*
 * decode_insn_format_3()
 */
static void decode_insn_format_3(struct code_element_opcode *ceo, uint32_t op)
{
	decode_general_operand(&ceo->dest, (op >> 16) & 0x7ff);
	decode_general_operand(&ceo->src1, op & 0x7ff);
	decode_reg_operand(&ceo->src2, (op >> 11) & 0xf);

	switch ((op >> 27) & 0x1f) {
	case 0x08:
		ceo->opcode = (op & 0x8000) ? OP_AND_1 : OP_AND_2;
		break;

	case 0x09:
		ceo->opcode = OP_AND_4;
		break;

	case 0x0a:
		ceo->opcode = (op & 0x8000) ? OP_OR_1 : OP_OR_2;
		break;

	case 0x0b:
		ceo->opcode = OP_OR_4;
		break;

	case 0x0c:
		ceo->opcode = (op & 0x8000) ? OP_XOR_1 : OP_XOR_2;
		break;

	case 0x0d:
		ceo->opcode = OP_XOR_4;
		break;

	case 0x0e:
		ceo->opcode = (op & 0x8000) ? OP_ADD_1 : OP_ADD_2;
		break;

	case 0x0f:
		ceo->opcode = OP_ADD_4;
		break;

	case 0x10:
		ceo->opcode = OP_ADDC;
		break;

	case 0x11:
		ceo->opcode = (op & 0x8000) ? OP_SUB_1 : OP_SUB_2;
		break;

	case 0x12:
		ceo->opcode = OP_SUB_4;
		break;

	case 0x13:
		ceo->opcode = OP_SUBC;
		break;

	case 0x14:
		ceo->opcode = (op & 0x8000) ? OP_PXBLEND_T : OP_PXBLEND;
		break;

	case 0x15:
		ceo->opcode = (op & 0x8000) ? OP_PXVI_S : OP_PXVI;
		break;

	case 0x16:
		ceo->opcode = (op & 0x8000) ? OP_PXADDS_U : OP_PXADDS;
		break;

	default:
		ceo->opcode = OP_UNUSED;
	}
}

/*
 * decode_insn_format_4()
 */
static void decode_insn_format_4(struct code_element_opcode *ceo, uint32_t op)
{
	decode_reg_operand(&ceo->dest, (op >> 16) & 0xf);
	decode_general_operand(&ceo->src1, op & 0x7ff);
	if (op & 0x04000000) {
		decode_reg_operand(&ceo->src2, (op >> 11) & 0xf);
	} else {
		decode_imm_operand(&ceo->src2, (op >> 11) & 0x1f);
	}

	int ext = (op >> 21) & 0x1f;
	switch (ext) {
	case 0x00:
		ceo->opcode = OP_PXHI;
		break;

	case 0x01:
		ceo->opcode = OP_MULS;
		break;

	case 0x02:
		ceo->opcode = OP_PXHI_S;
		break;

	case 0x03:
		ceo->opcode = OP_MULU;
		break;

	case 0x05:
		ceo->opcode = OP_MULF;
		break;

	case 0x06:
		ceo->opcode = OP_BTST;
		break;

	case 0x08:
		ceo->opcode = OP_CRCGEN;
		break;

	case 0x09:
		ceo->opcode = OP_MAC;
		break;

	case 0x0a:
		ceo->opcode = OP_LSL_1;
		break;

	case 0x0b:
		ceo->opcode = OP_LSR_1;
		break;

	case 0x0c:
		ceo->opcode = OP_ASR_1;
		break;

	case 0x10:
		ceo->opcode = OP_LSL_4;
		break;

	case 0x11:
		ceo->opcode = OP_LSL_2;
		break;

	case 0x12:
		ceo->opcode = OP_LSR_4;
		break;

	case 0x13:
		ceo->opcode = OP_LSR_2;
		break;

	case 0x14:
		ceo->opcode = OP_ASR_4;
		break;

	case 0x15:
		ceo->opcode = OP_ASR_2;
		break;

	case 0x16:
		ceo->opcode = OP_BFEXTU;
		break;

	case 0x18:
		ceo->opcode = OP_BFRVRS;
		break;

	case 0x1a:
		ceo->opcode = OP_SHFTD;
		break;

	case 0x1c:
		ceo->opcode = OP_MERGE;
		break;

	case 0x1e:
		ceo->opcode = OP_SHMRG_2;
		break;

	case 0x1f:
		ceo->opcode = OP_SHMRG_1;
		break;

	default:
		ceo->opcode = OP_UNUSED;
	}
}

/*
 * decode_insn_format_5()
 */
static void decode_insn_format_5(struct code_element_opcode *ceo, uint32_t op)
{
	decode_general_operand(&ceo->src1, op & 0x7ff);
	decode_imm_operand(&ceo->src2, (op >> 11) & 0xffff);

	ceo->opcode = OP_CMPI;
}

/*
 * decode_insn_format_6()
 */
static void decode_insn_format_6(struct code_element_opcode *ceo, uint32_t op)
{
	decode_general_operand(&ceo->dest, (op >> 16) & 0x7ff);
	decode_imm_operand(&ceo->src1, (int16_t)(op & 0xffff));

	ceo->opcode = OP_MOVEI;
}

static enum opcodes jmp_ops[64] = {
	OP_JMPF_S_F,
	OP_JMPF_W_F,
	OP_JMPF_S_T,
	OP_JMPF_W_T,
	OP_JMPLO_S_F,
	OP_JMPLO_W_F,
	OP_JMPLO_S_T,
	OP_JMPLO_W_T,
	OP_JMPHS_S_F,
	OP_JMPHS_W_F,
	OP_JMPHS_S_T,
	OP_JMPHS_W_T,
	OP_JMPEQ_S_F,
	OP_JMPEQ_W_F,
	OP_JMPEQ_S_T,
	OP_JMPEQ_W_T,
	OP_JMPGE_S_F,
	OP_JMPGE_W_F,
	OP_JMPGE_S_T,
	OP_JMPGE_W_T,
	OP_JMPGT_S_F,
	OP_JMPGT_W_F,
	OP_JMPGT_S_T,
	OP_JMPGT_W_T,
	OP_JMPHI_S_F,
	OP_JMPHI_W_F,
	OP_JMPHI_S_T,
	OP_JMPHI_W_T,
	OP_JMPLE_S_F,
	OP_JMPLE_W_F,
	OP_JMPLE_S_T,
	OP_JMPLE_W_T,
	OP_JMPLS_S_F,
	OP_JMPLS_W_F,
	OP_JMPLS_S_T,
	OP_JMPLS_W_T,
	OP_JMPLT_S_F,
	OP_JMPLT_W_F,
	OP_JMPLT_S_T,
	OP_JMPLT_W_T,
	OP_JMPMI_S_F,
	OP_JMPMI_W_F,
	OP_JMPMI_S_T,
	OP_JMPMI_W_T,
	OP_JMPNE_S_F,
	OP_JMPNE_W_F,
	OP_JMPNE_S_T,
	OP_JMPNE_W_T,
	OP_JMPPL_S_F,
	OP_JMPPL_W_F,
	OP_JMPPL_S_T,
	OP_JMPPL_W_T,
	OP_JMPT_S_F,
	OP_JMPT_W_F,
	OP_JMPT_S_T,
	OP_JMPT_W_T,
	OP_JMPVC_S_F,
	OP_JMPVC_W_F,
	OP_JMPVC_S_T,
	OP_JMPVC_W_T,
	OP_JMPVS_S_F,
	OP_JMPVS_W_F,
	OP_JMPVS_S_T,
	OP_JMPVS_W_T
};

/*
 * decode_insn_format_7()
 */
static void decode_insn_format_7(struct code_element_opcode *ceo, uint32_t op)
{
	int ival = op & 0x100000 ? -1 : 0;
	ival &= (~0xfffff);
	ival |= op & 0xfffff;
	decode_imm_operand(&ceo->src1, ival);

	ceo->opcode = jmp_ops[(op >> 21) & 0x3f];
}

/*
 * decode_insn_format_8()
 */
static void decode_insn_format_8(struct code_element_opcode *ceo, uint32_t op)
{
	decode_reg_operand(&ceo->dest, ((op & 0x00e00000) >> 21) + 32);
	decode_imm_operand(&ceo->src1, op & 0x1fffff | ((op & 0x07000000) >> 3));

	switch ((op >> 27) & 0x1f) {
	case 0x1b:
		{
			int ival = ceo->src1.value.oi.imm & 0x800000 ? -1 : 0;
			ival &= (~0x7fffff);
			ceo->src1.value.oi.imm = ival | ceo->src1.value.oi.imm & 0x7fffff;
			ceo->opcode = OP_CALL;
		}
		break;

	case 0x1c:
		ceo->opcode = OP_MOVEAI;
		break;

	default:
		ceo->opcode = OP_UNUSED;
	}
}

/*
 * decode_insn_format_9()
 */
static void decode_insn_format_9(struct code_element_opcode *ceo, uint32_t op)
{
	decode_reg_operand(&ceo->dest, ((op & 0x00e00000) >> 21) + 32);
	struct operand *operand = &ceo->src1;
	operand->mode = OM_AREG_PLUS_IMM;
	operand->value.oapi.areg = 0x20 + ((op >> 5) & 0x7);
	operand->value.oapi.imm = (op & 0x1f) | ((op >> 3) & 0xe0) | ((op >> 8) & 0x1f00) | ((op >> 11) & 0xe000);

	ceo->opcode = OP_CALLI;
}

/*
 * decode_insn_format_10()
 */
static void decode_insn_format_10(struct code_element_opcode *ceo, uint32_t op)
{
	decode_reg_operand(&ceo->dest, (op & 0x10000) ? 0x101 : 0x100);
	decode_general_operand(&ceo->src1, op & 0x7ff);
	decode_reg_operand(&ceo->src2, (op >> 11) & 0xf);
	if (op & 0x04000000) {
		decode_reg_operand(&ceo->src2, (op >> 11) & 0xf);
	} else {
		decode_imm_operand(&ceo->src2, (op >> 11) & 0x1f);
	}

	int ext = (op >> 21) & 0x1f;
	switch (ext) {
	case 0x00:
		ceo->opcode = (op & 0x80000) ? OP_MULS_T : OP_MULS;
		break;

	case 0x01:
		ceo->opcode = (op & 0x100000) ?
				((op & 0x80000) ? OP_MACS_C_T : OP_MACS_C) :
				((op & 0x80000) ? OP_MACS_T : OP_MACS);
		break;

	case 0x02:
		ceo->opcode = (op & 0x80000) ? OP_MULU_T : OP_MULU;
		break;

	case 0x03:
		ceo->opcode = (op & 0x100000) ?
				((op & 0x80000) ? OP_MACU_C_T : OP_MACU_C) :
				((op & 0x80000) ? OP_MACU_T : OP_MACU);
		break;

	case 0x04:
		ceo->opcode = (op & 0x100000) ?
				((op & 0x80000) ? OP_MULF_C_T : OP_MULF_C) :
				((op & 0x80000) ? OP_MULF_T : OP_MULF);
		break;

	case 0x05:
		ceo->opcode = (op & 0x100000) ?
				((op & 0x80000) ? OP_MACF_C_T : OP_MACF_C) :
				((op & 0x80000) ? OP_MACF_T : OP_MACF);
		break;

	case 0x07:
		ceo->opcode = (op & 0x100000) ?
				((op & 0x80000) ? OP_MACUS_C_T : OP_MACUS_C) :
				((op & 0x80000) ? OP_MACUS_T : OP_MACUS);
		break;

	case 0x08:
		ceo->opcode = OP_MULS_4;
		break;

	case 0x09:
		ceo->opcode = (op & 0x100000) ?
				((op & 0x80000) ? OP_MSUF_C_T : OP_MSUF_C) :
				((op & 0x80000) ? OP_MSUF_T : OP_MSUF);
		break;

	case 0x0a:
		ceo->opcode = OP_MULU_4;
		break;

	case 0x10:
		ceo->opcode = (op & 0x100000) ?
				((op & 0x80000) ? OP_MADD_4_C_T : OP_MADD_4_C) :
				((op & 0x80000) ? OP_MADD_4_T : OP_MADD_4);
		break;

	case 0x11:
		ceo->opcode = (op & 0x100000) ?
				((op & 0x80000) ? OP_MADD_2_C_T : OP_MADD_2_C) :
				((op & 0x80000) ? OP_MADD_2_T : OP_MADD_2);
		break;

	case 0x12:
		ceo->opcode = (op & 0x100000) ?
				((op & 0x80000) ? OP_MSUB_4_C_T : OP_MSUB_4_C) :
				((op & 0x80000) ? OP_MSUB_4_T : OP_MSUB_4);
		break;

	case 0x13:
		ceo->opcode = (op & 0x100000) ?
				((op & 0x80000) ? OP_MSUB_2_C_T : OP_MSUB_2_C) :
				((op & 0x80000) ? OP_MSUB_2_T : OP_MSUB_2);
		break;

	default:
		ceo->opcode = OP_UNUSED;
	}
}

/*
 * decode_insn()
 */
static void decode_insn(struct code_element_opcode *ceo)
{
	uint32_t op = ceo->instr;

	/*
	 * Split into opcode formats.
	 */
	int op_group = op >> 27;
	switch (op_group) {
	case 0x00:
		decode_insn_format_1(ceo, op);
		break;

	case 0x04:
	case 0x05:
		decode_insn_format_2(ceo, op);
		break;

	case 0x08:
	case 0x09:
	case 0x0a:
	case 0x0b:
	case 0x0c:
	case 0x0d:
	case 0x0e:
	case 0x0f:
	case 0x10:
	case 0x11:
	case 0x12:
	case 0x13:
	case 0x14:
	case 0x15:
	case 0x16:
		decode_insn_format_3(ceo, op);
		break;

	case 0x02:
		decode_insn_format_4(ceo, op);
		break;

	case 0x18:
		decode_insn_format_5(ceo, op);
		break;

	case 0x19:
		decode_insn_format_6(ceo, op);
		break;

	case 0x1a:
		decode_insn_format_7(ceo, op);
		break;

	case 0x1b:
	case 0x1c:
		decode_insn_format_8(ceo, op);
		break;

	case 0x1e:
		decode_insn_format_9(ceo, op);
		break;

	case 0x06:
		decode_insn_format_10(ceo, op);
		break;

	default:
		ceo->opcode = OP_UNUSED;
	}
}

/*
 * insert_block_separator_after()
 */
static struct code_element_block_separator *insert_block_separator_after(struct code_element *ce)
{
	struct code_element_block_separator *b
		= (struct code_element_block_separator *)malloc(sizeof(struct code_element_block_separator));
	if (!b) {
		fprintf(stderr, "unable to allocate block separator\n");
		return NULL;
	}

	b->ce.type = ET_BLOCK_SEPARATOR;

	/*
	 * Link ourselves into the stream.  Note that we can never be the head or tail of the
	 * code element stream so we can do this very simply!
	 */
	b->ce.next = ce->next;
	b->ce.prev = ce;
	ce->next->prev = &b->ce;
	ce->next = &b->ce;

	return b;
}

/*
 * compute_label()
 */
static struct code_element_label *compute_label(struct code_element_opcode *ceo, int offset, bool call_label)
{
	struct code_element *ce = (struct code_element *)ceo;
	struct code_element *d = ce;
	uint32_t addr = ceo->addr + offset;

	/*
	 * First look to see if we already have a label in the label hash.
	 */
	int hash_idx = (addr >> 2) & (LABEL_HASH_SLOTS - 1);
	struct code_element_label *lhash = label_hash[hash_idx];
	while (lhash) {
		if (lhash->addr == addr) {
			lhash->refs++;
			ceo->label = lhash;
			return lhash;
		}

		lhash = lhash->hash_next;
	}

	/*
	 * If the offset is negative walk backwards.
	 */
	if (offset < 0) {
		uint32_t daddr = 0xfffffffc;
		do {
			d = d->prev;
			if (d && d->type == ET_OPCODE) {
				struct code_element_opcode *dceo = (struct code_element_opcode *)d;
				daddr = dceo->addr;
			}
		} while (d && (daddr != addr));

		if ((!d) || (daddr != addr)) {
			fprintf(stderr, "could not compute label: %lx\n", (unsigned long)ce);
			return NULL;
		}
	} else if (offset > 0) {
		uint32_t daddr = 0;
		do {
			d = d->next;
			if (d && d->type == ET_OPCODE) {
				struct code_element_opcode *dceo = (struct code_element_opcode *)d;
				daddr = dceo->addr;
			}
		} while (d && (daddr != addr));

		if ((!d) || (daddr != addr)) {
			fprintf(stderr, "could not compute label: %lx\n", (unsigned long)ce);
			return NULL;
		}
	}

	/*
	 * Create and insert the new label.
	 */
	struct code_element_label *label
		= (struct code_element_label *)malloc(sizeof(struct code_element_label));
	if (!label) {
		fprintf(stderr, "unable to allocate label\n");
		return NULL;
	}

	label->ce.type = ET_LABEL;
	label->addr = addr;
	label->refs = 1;
	label->ce.next = d;
	label->ce.prev = d->prev;
	label->call_label = call_label;
	if (d->prev) {
		d->prev->next = &label->ce;
	}
	d->prev = &label->ce;

	/*
	 * Insert ourself into the label hash.
	 */
	label->hash_next = label_hash[hash_idx];
	label_hash[hash_idx] = label;
	ceo->label = label;

	return label;
}

/*
 * insert_labels_and_separators()
 */
static bool insert_labels_and_separators(void)
{
	/*
	 * Walk the instruction stream, inserting jump labels and block separators.
	 */
	struct code_element *ce = code_head->next;
	while (ce != code_tail) {
		if (ce->type == ET_OPCODE) {
			struct code_element_opcode *ceo = (struct code_element_opcode *)ce;
			enum opcodes o = ceo->opcode;

			/*
			 * Look to see if we have an instruction with a label reference.  If we do then
			 * try to insert that label.
			 */
			if ((o == OP_CALL) || (o >= OP_JMPF_S_F && o <= OP_JMPVS_W_T)) {
				struct code_element_label *label
					= compute_label(ceo, ceo->src1.value.oi.imm * 4, o == OP_CALL);
#if 0
				if (!label) {
					fprintf(stderr, "failed to compute label for opcode: 0x%02x\n", o);
					return false;
				}
#endif
			}

			/*
			 * Does our instruction terminate a basic block?  If yes then insert a separator.
			 */
			if ((o >= OP_JMPF_S_F && o <= OP_JMPVS_W_T)
					|| o == OP_RET
					|| o == OP_CALL
					|| o == OP_CALLI
					|| o == OP_SYSCALL
					|| o == OP_SYSRET) {
				if (ceo->ce.next) {
					if (!insert_block_separator_after(&ceo->ce)) {
						fprintf(stderr, "failed to insert block separator");
						return false;
					}

					/*
					 * Ensure we skip the block separator we just inserted.
					 */
					ce = ce->next;
				}
			}
		}

		ce = ce->next;
	}

	return true;
}

/*
 * number_labels()
 */
static bool number_labels(void)
{
	int lnum = 1;

	/*
	 * Walk the instruction stream, numerically ordering labels.
	 */
	struct code_element *ce = code_head->next;
	while (ce != code_tail) {
		switch (ce->type) {
		case ET_LABEL:
			{
				struct code_element_label *cel = (struct code_element_label *)ce;
				cel->number = lnum++;

				cel->name = (char *)malloc(32 * sizeof(char));
				if (!cel->name) {
					fprintf(stderr, "failed to insert label name");
					return false;
				}

				sprintf(cel->name, "L%d", cel->number);
			}
			break;

		case ET_OPCODE:
			{
				struct code_element_opcode *ceo = (struct code_element_opcode *)ce;
				opcode_counts[ceo->opcode]++;
				total_insns++;
			}
			break;

		case ET_SYMBOL:
			{
				struct code_element_symbol *ces = (struct code_element_symbol *)ce;
				if (elf_strings && ces->str_offs) {
					ces->name = elf_strings + ces->str_offs; 
				} else {
					ces->name = "UNDEF";
				}
			}
			break;
		}

		ce = ce->next;
	}

	return true;
}

/*
 * code_unpack()
 */
static bool code_unpack(uint32_t addr, uint32_t *insns, int count)
{
	struct code_element *orig_head = code_tail->prev;
	struct code_element *orig_tail = code_tail;
	struct code_element *head = orig_head;
	struct code_element *tail = orig_tail;
	uint32_t *op = insns;

	for (int i = 0; i < count; i++, op++) {
		struct code_element_opcode *ceo = (struct code_element_opcode *)malloc(sizeof(struct code_element_opcode));
		if (!ceo) {
			fprintf(stderr, "Failed to alloc space for opcode\n");
			return false;
		}

		struct code_element *ce = (struct code_element *)ceo;
		head->next = ce;
		ce->prev = head;
		tail->prev = ce;
		ce->next = tail;

		ce->type = ET_OPCODE;

		ceo->instr = elf_uint32_to_host(*op);
		ceo->addr = addr;
		addr += 4;
		ceo->label = NULL;
		ceo->src1.mode = OM_UNUSED;
		ceo->src2.mode = OM_UNUSED;
		ceo->dest.mode = OM_UNUSED;

		decode_insn(ceo);

		head = ce;
	}

	return true;
}

/*
 * symbol_unpack()
 */
static bool symbol_unpack(struct elf32_sym *syms, int count)
{
	struct code_element *orig_head = code_tail->prev;
	struct code_element *orig_tail = code_tail;
	struct code_element *head = orig_head;
	struct code_element *tail = orig_tail;
	struct elf32_sym *es = syms;

	for (int i = 0; i < count; i++, es++) {
		es->st_name = elf_uint32_to_host(es->st_name);
		es->st_value = elf_uint32_to_host(es->st_value);
		es->st_size = elf_uint32_to_host(es->st_size);
		es->st_shndx = elf_uint16_to_host(es->st_shndx);

		struct code_element_symbol *ces = (struct code_element_symbol *)malloc(sizeof(struct code_element_symbol));
		if (!ces) {
			fprintf(stderr, "Failed to alloc space for symbol\n");
			return false;
		}

		struct code_element *ce = (struct code_element *)ces;
		head->next = ce;
		ce->prev = head;
		tail->prev = ce;
		ce->next = tail;

		ce->type = ET_SYMBOL;
		ces->type = es->st_info & 0xf;
		ces->addr = es->st_value;
		ces->section = es->st_shndx;
		ces->str_offs = es->st_name;

		head = ce;
	}

	return true;
}

/*
 * section_unpack()
 */
static bool section_unpack(struct elf32_shdr *es, unsigned char *fbuf, int section_num)
{
	es->sh_name = elf_uint32_to_host(es->sh_name);
	es->sh_type = elf_uint32_to_host(es->sh_type);
	es->sh_flags = elf_uint32_to_host(es->sh_flags);
	es->sh_addr = elf_uint32_to_host(es->sh_addr);
	es->sh_offset = elf_uint32_to_host(es->sh_offset);
	es->sh_size = elf_uint32_to_host(es->sh_size);
	es->sh_link = elf_uint32_to_host(es->sh_link);
	es->sh_info = elf_uint32_to_host(es->sh_info);
	es->sh_addralign = elf_uint32_to_host(es->sh_addralign);
	es->sh_entsize = elf_uint32_to_host(es->sh_entsize);

	/*
	 * Do we have a section header strings section?  If not then this is probably the strings section!
	 */
	if (!elf_shstrings) {
		if (elf_hdr->e_shstrndx != SHN_UNDEF) {
			elf_shstrings = (char *)(fbuf + es->sh_offset);
		}
	}

	struct code_element_section *ces = (struct code_element_section *)malloc(sizeof(struct code_element_section));
	if (!ces) {
		fprintf(stderr, "Failed to alloc space for section\n");
		return false;
	}

	/*
	 * Insert ourself into the section hash.
	 */
	int hash_idx = section_num % SECTION_HASH_SLOTS;
	ces->hash_next = section_hash[hash_idx];
	section_hash[hash_idx] = ces;
	ces->number = section_num;

	struct code_element *ce = (struct code_element *)ces;
	ce->type = ET_SECTION;
	if (elf_shstrings) {
		ces->name = elf_shstrings + es->sh_name; 
	} else {
		ces->name = "UNDEF";
	}

	ce->next = code_tail;
	ce->prev = code_tail->prev;
	code_tail->prev->next = ce;
	code_tail->prev = ce;

	/*
	 * If this section includes code then unpack it.
	 */
	if (es->sh_flags & SHF_EXECINSTR) {
		if (!code_unpack(es->sh_addr, (uint32_t *)(fbuf + es->sh_offset), es->sh_size / 4)) {
			fprintf(stderr, "Failed to unpack instructions\n");
			return false;
		}
	}

	/*
	 * If this section includes symbol table data then unpack it.
	 */
	if (es->sh_type == SHT_SYMTAB || es->sh_type == SHT_DYNSYM) {
		if (es->sh_type == SHT_DYNSYM) {
			dynsym_section = section_num;
		} else {
			symtab_section = section_num;
		}

		if (es->sh_entsize != sizeof(struct elf32_sym)) {
			fprintf(stderr, "Symbol table entry size: %d - expected: %ld\n",
				es->sh_entsize, sizeof(struct elf32_sym));
			return false;
		}
		if (!symbol_unpack((struct elf32_sym *)(fbuf + es->sh_offset), es->sh_size / es->sh_entsize)) {
			fprintf(stderr, "Failed to unpack symbols\n");
			return false;
		}
	}

	/*
	 * If this section includes a string table then reference it.
	 */
	if (es->sh_type == SHT_STRTAB) {
		elf_strings = (char *)(fbuf + es->sh_offset);
	}

	return true;
}

/*
 * elf_unpack()
 */
static bool elf_unpack(const char *filename)
{
	int f = open(filename, O_RDONLY);
	if (!f) {
		fprintf(stderr, "Can't open file: %s\n", filename);
		return false;
	}

	/*
	 * How large is our file?
	 */
	struct stat st;
	if (fstat(f, &st) < 0) {
		fprintf(stderr, "Failed to stat file: %s\n", filename);
		return false;
	}

	unsigned char *fbuf = (unsigned char *)malloc(st.st_size);
	if (!fbuf) {
		fprintf(stderr, "Failed to make space for file: %s\n", filename);
		return false;
	}

	/*
	 * Start by reading the whole file into memory.
	 */
	if (read(f, fbuf, st.st_size) < 0) {
		fprintf(stderr, "Can't read file: %s\n", filename);
		return false;
	}

	/*
	 * Let's look at the header - check this is an ELF file.
	 */
	elf_hdr = (struct elf32_hdr *)fbuf;
	struct elf32_hdr *eh = elf_hdr;
	if (eh->e_ident[EI_MAG0] != 0x7f
			|| eh->e_ident[EI_MAG1] != 'E'
			|| eh->e_ident[EI_MAG2] != 'L'
			|| eh->e_ident[EI_MAG3] != 'F') {
		fprintf(stderr, "Not an ELF file: %s\n", filename);
		return false;
	}

	eh->e_type = elf_uint16_to_host(eh->e_type);
	eh->e_machine = elf_uint16_to_host(eh->e_machine);
	eh->e_version = elf_uint32_to_host(eh->e_version);
	eh->e_entry = elf_uint32_to_host(eh->e_entry);
	eh->e_phoff = elf_uint32_to_host(eh->e_phoff);
	eh->e_shoff = elf_uint32_to_host(eh->e_shoff);
	eh->e_flags = elf_uint32_to_host(eh->e_flags);
	eh->e_ehsize = elf_uint16_to_host(eh->e_ehsize);
	eh->e_phentsize = elf_uint16_to_host(eh->e_phentsize);
	eh->e_phnum = elf_uint16_to_host(eh->e_phnum);
	eh->e_shentsize = elf_uint16_to_host(eh->e_shentsize);
	eh->e_shnum = elf_uint16_to_host(eh->e_shnum);
	eh->e_shstrndx = elf_uint16_to_host(eh->e_shstrndx);

	if (eh->e_ehsize != sizeof(struct elf32_hdr)) {
		fprintf(stderr, "ELF header: e_ehsize: %d bytes - expected: %ld bytes\n",
			eh->e_ehsize, sizeof(struct elf32_hdr));
		return false;
	}

	if (eh->e_shentsize != sizeof(struct elf32_shdr)) {
		fprintf(stderr, "ELF header: e_shentsize: %d bytes - expected: %ld bytes\n",
			eh->e_shentsize, sizeof(struct elf32_shdr));
		return false;
	}

	// XXX - add validation of offsets.

	/*
	 * Insert dummy section markers so we never have an empty list.
	 */
	struct code_element_section *ces_h = (struct code_element_section *)malloc(sizeof(struct code_element_section));
	if (!ces_h) {
		fprintf(stderr, "Failed to alloc space for section\n");
		return false;
	}

	struct code_element *ce_h = (struct code_element *)ces_h;
	ce_h->type = ET_SECTION;
	ces_h->name = "START";
	ces_h->number = -1;

	struct code_element_section *ces_t = (struct code_element_section *)malloc(sizeof(struct code_element_section));
	if (!ces_t) {
		fprintf(stderr, "Failed to alloc space for section\n");
		return false;
	}

	struct code_element *ce_t = (struct code_element *)ces_t;
	ce_t->type = ET_SECTION;
	ces_t->name = "END";
	ces_t->number = -1;

	ce_h->next = ce_t;
	ce_h->prev = NULL;
	code_head = ce_h;

	ce_t->next = NULL;
	ce_t->prev = ce_h;
	code_tail = ce_t;

	struct elf32_shdr *sh = (struct elf32_shdr *)(fbuf + eh->e_shoff);
	if (!section_unpack(&sh[eh->e_shstrndx], fbuf, eh->e_shstrndx)) {
		fprintf(stderr, "Unable to unpack string section\n");
		return false;
	}

	for (int i = 1; i < eh->e_shnum; i++) {
		if (i == eh->e_shstrndx) {
			continue;
		}

		if (!section_unpack(&sh[i], fbuf, i)) {
			fprintf(stderr, "Unable to unpack string section\n");
			return false;
		}
	}

	if (!insert_labels_and_separators()) {
		fprintf(stderr, "Failed to insert labels and separators\n");
		return false;
	}

	number_labels();

	return true;
}

/*
 * dump_operand()
 */
static void dump_operand(struct operand *o, int scale)
{
	switch (o->mode) {
	case OM_AREG_PLUS_IMM:
		if (!o->value.oapi.imm) {
			printf("(%s)", regname[o->value.oapi.areg]);
		} else {
			printf("%d(%s)", o->value.oapi.imm * scale, regname[o->value.oapi.areg]);
		}
		break;

	case OM_AREG_PLUS_DREG:
		printf("(%s,%s)", regname[o->value.oapd.areg], regname[o->value.oapd.dreg]);
		break;

	case OM_AREG_MODIFY:
		{
			int mval = o->value.oam.modify & 0x8 ? -1 : 0;
			mval &= (~0x7);
			mval |= o->value.oam.modify & 0x7;
			if (o->value.oam.modify & 0x10) {
				printf("%d(%s)++", mval * scale, regname[o->value.oam.areg]);
			} else {
				printf("(%s)%d++", regname[o->value.oam.areg], mval * scale);
			}
		}
		break;

	case OM_REG:
		printf("%s", regname[o->value.or.reg]);
		break;

	case OM_IMM:
		printf("#%d", o->value.oi.imm);
		break;

	case OM_UNUSED:
		printf("unknown");
	}
}

/*
 * dump_code_opcode_0op()
 */
static void dump_code_opcode_0op(struct code_element_opcode *ceo)
{
	printf("%s", opcodes[ceo->opcode].name);
}

/*
 * dump_code_opcode_1op()
 */
static void dump_code_opcode_1op(struct code_element_opcode *ceo)
{
	struct opcode *o = &opcodes[ceo->opcode];

	printf("%s ", o->name);
	dump_operand(&ceo->src1, o->size);
}

/*
 * dump_code_opcode_2op()
 */
static void dump_code_opcode_2op(struct code_element_opcode *ceo)
{
	struct opcode *o = &opcodes[ceo->opcode];

	printf("%s ", o->name);
	dump_operand(&ceo->dest, o->size);
	printf(", ");
	dump_operand(&ceo->src1, o->size);
}

/*
 * dump_code_opcode_3op()
 */
static void dump_code_opcode_3op(struct code_element_opcode *ceo)
{
	struct opcode *o = &opcodes[ceo->opcode];

	printf("%s ", o->name);
	dump_operand(&ceo->dest, o->size);
	printf(", ");
	dump_operand(&ceo->src1, o->size);
	printf(", ");
	dump_operand(&ceo->src2, o->size);
}

/*
 * dump_code_opcode_cmpi()
 */
static void dump_code_opcode_cmpi(struct code_element_opcode *ceo)
{
	struct opcode *o = &opcodes[ceo->opcode];

	printf("%s ", o->name);
	dump_operand(&ceo->src1, o->size);
	printf(", ");
	dump_operand(&ceo->src2, o->size);
}

/*
 * dump_code_opcode_call()
 */
static void dump_code_opcode_call(struct code_element_opcode *ceo)
{
	struct opcode *o = &opcodes[ceo->opcode];

	printf("%s ", o->name);
	dump_operand(&ceo->dest, o->size);
	if (ceo->label) {
		printf(", %s ; offset ", ceo->label->name);
	} else {
		printf(", <unknown> ; offset ");
	}
	dump_operand(&ceo->src1, o->size);
}

/*
 * dump_code_opcode_jmp()
 */
static void dump_code_opcode_jmp(struct code_element_opcode *ceo)
{
	struct opcode *o = &opcodes[ceo->opcode];

	if (ceo->label) {
		printf("%s %s ; offset ", o->name, ceo->label->name);
	} else {
		printf(", <unknown> ; offset ");
	}
	dump_operand(&ceo->src1, o->size);
}

/*
 * dump_code_opcode()
 */
static void dump_code_opcode(struct code_element *ce)
{
	struct code_element_opcode *ceo = (struct code_element_opcode *)ce;

	printf("  %08x: ", ceo->addr);

	switch (ceo->opcode) {
	case OP_BKPT:
	case OP_SUSPEND:
		dump_code_opcode_0op(ceo);
		break;

	case OP_FLUSH:
	case OP_IREAD:
	case OP_PREFETCH:
	case OP_RET:
	case OP_SETCSR:
	case OP_SYNC:
	case OP_SYSCALL:
	case OP_SYSRET:
		dump_code_opcode_1op(ceo);
		break;
	
	case OP_BFRVRS:
	case OP_BTST:
	case OP_CALLI:
	case OP_CRCGEN:
	case OP_IWRITE:
	case OP_EXT_1:
	case OP_EXT_2:
	case OP_LEA_1:
	case OP_LEA_2:
	case OP_LEA_4:
	case OP_MAC:
	case OP_MOVE_1:
	case OP_MOVE_2:
	case OP_MOVE_4:
	case OP_MOVEA:
	case OP_MOVEAI:
	case OP_MOVEI:
	case OP_NOT_2:
	case OP_NOT_4:
	case OP_PDEC:
	case OP_PXCNV:
	case OP_PXCNV_T:
	case OP_SWAPB_2:
	case OP_SWAPB_4:
		dump_code_opcode_2op(ceo);
		break;

	case OP_ADD_1:
	case OP_ADD_2:
	case OP_ADD_4:
	case OP_ADDC:
	case OP_AND_1:
	case OP_AND_2:
	case OP_AND_4:
	case OP_ASR_1:
	case OP_ASR_2:
	case OP_ASR_4:
	case OP_BCLR:
	case OP_BFEXTU:
	case OP_BSET:
	case OP_LSL_1:
	case OP_LSL_2:
	case OP_LSL_4:
	case OP_LSR_1:
	case OP_LSR_2:
	case OP_LSR_4:
	case OP_MADD_2:
	case OP_MADD_2_C:
	case OP_MADD_2_T:
	case OP_MADD_2_C_T:
	case OP_MADD_4:
	case OP_MADD_4_C:
	case OP_MADD_4_T:
	case OP_MADD_4_C_T:
	case OP_MACF:
	case OP_MACF_C:
	case OP_MACF_T:
	case OP_MACF_C_T:
	case OP_MACS:
	case OP_MACS_C:
	case OP_MACS_T:
	case OP_MACS_C_T:
	case OP_MACU:
	case OP_MACU_C:
	case OP_MACU_T:
	case OP_MACU_C_T:
	case OP_MACUS:
	case OP_MACUS_C:
	case OP_MACUS_T:
	case OP_MACUS_C_T:
	case OP_MERGE:
	case OP_MSUB_2:
	case OP_MSUB_2_C:
	case OP_MSUB_2_T:
	case OP_MSUB_2_C_T:
	case OP_MSUB_4:
	case OP_MSUB_4_C:
	case OP_MSUB_4_T:
	case OP_MSUB_4_C_T:
	case OP_MSUF:
	case OP_MSUF_C:
	case OP_MSUF_T:
	case OP_MSUF_C_T:
	case OP_MULF:
	case OP_MULF_C:
	case OP_MULF_T:
	case OP_MULF_C_T:
	case OP_MULS:
	case OP_MULS_T:
	case OP_MULS_4:
	case OP_MULU:
	case OP_MULU_T:
	case OP_MULU_4:
	case OP_OR_1:
	case OP_OR_2:
	case OP_OR_4:
	case OP_PXADDS:
	case OP_PXADDS_U:
	case OP_PXBLEND:
	case OP_PXBLEND_T:
	case OP_PXHI:
	case OP_PXHI_S:
	case OP_PXVI:
	case OP_PXVI_S:
	case OP_SHFTD:
	case OP_SHMRG_1:
	case OP_SHMRG_2:
	case OP_SUB_1:
	case OP_SUB_2:
	case OP_SUB_4:
	case OP_SUBC:
	case OP_XOR_1:
	case OP_XOR_2:
	case OP_XOR_4:
		dump_code_opcode_3op(ceo);
		break;

	case OP_JMPF_S_F:
	case OP_JMPLO_S_F:
	case OP_JMPHS_S_F:
	case OP_JMPEQ_S_F:
	case OP_JMPGE_S_F:
	case OP_JMPGT_S_F:
	case OP_JMPHI_S_F:
	case OP_JMPLE_S_F:
	case OP_JMPLS_S_F:
	case OP_JMPLT_S_F:
	case OP_JMPMI_S_F:
	case OP_JMPNE_S_F:
	case OP_JMPPL_S_F:
	case OP_JMPT_S_F:
	case OP_JMPVC_S_F:
	case OP_JMPVS_S_F:
	case OP_JMPF_S_T:
	case OP_JMPLO_S_T:
	case OP_JMPHS_S_T:
	case OP_JMPEQ_S_T:
	case OP_JMPGE_S_T:
	case OP_JMPGT_S_T:
	case OP_JMPHI_S_T:
	case OP_JMPLE_S_T:
	case OP_JMPLS_S_T:
	case OP_JMPLT_S_T:
	case OP_JMPMI_S_T:
	case OP_JMPNE_S_T:
	case OP_JMPPL_S_T:
	case OP_JMPT_S_T:
	case OP_JMPVC_S_T:
	case OP_JMPVS_S_T:
	case OP_JMPF_W_F:
	case OP_JMPLO_W_F:
	case OP_JMPHS_W_F:
	case OP_JMPEQ_W_F:
	case OP_JMPGE_W_F:
	case OP_JMPGT_W_F:
	case OP_JMPHI_W_F:
	case OP_JMPLE_W_F:
	case OP_JMPLS_W_F:
	case OP_JMPLT_W_F:
	case OP_JMPMI_W_F:
	case OP_JMPNE_W_F:
	case OP_JMPPL_W_F:
	case OP_JMPT_W_F:
	case OP_JMPVC_W_F:
	case OP_JMPVS_W_F:
	case OP_JMPF_W_T:
	case OP_JMPLO_W_T:
	case OP_JMPHS_W_T:
	case OP_JMPEQ_W_T:
	case OP_JMPGE_W_T:
	case OP_JMPGT_W_T:
	case OP_JMPHI_W_T:
	case OP_JMPLE_W_T:
	case OP_JMPLS_W_T:
	case OP_JMPLT_W_T:
	case OP_JMPMI_W_T:
	case OP_JMPNE_W_T:
	case OP_JMPPL_W_T:
	case OP_JMPT_W_T:
	case OP_JMPVC_W_T:
	case OP_JMPVS_W_T:
		dump_code_opcode_jmp(ceo);
		break;

	case OP_CALL:
		dump_code_opcode_call(ceo);
		break;

	case OP_CMPI:
		dump_code_opcode_cmpi(ceo);
		break;

	default:
		printf("%d", ceo->opcode);
	}
	printf("\n");
}

/*
 * dump_elf()
 */
static void dump_elf(void)
{
	struct code_element *ce = code_head;

	while (ce) {
		switch (ce->type) {
		case ET_OPCODE:
			dump_code_opcode(ce);
			break;

		case ET_LABEL:
			{
				struct code_element_label *cel = (struct code_element_label *)ce;
				if (ce->prev && ce->prev->type == ET_OPCODE) {
					printf("\n");
				}
				if (cel->call_label) {
					printf("CL%d: ; refs: %d\n", cel->number, cel->refs);
				} else {
					printf("JL%d: ; refs: %d\n", cel->number, cel->refs);
				}
			}
			break;

		case ET_BLOCK_SEPARATOR:
			printf("\n");
			break;

		case ET_SECTION:
			{
				struct code_element_section *ces = (struct code_element_section *)ce;
				printf("section: %s (%d)\n\n", ces->name, ces->number);
			}
			break;

		case ET_SYMBOL:
			{
				struct code_element_symbol *ces = (struct code_element_symbol *)ce;
				printf("symbol: %s - %d @ %08x, sect: %d\n", ces->name, ces->type, ces->addr, ces->section);
			}
			break;
		}

		ce = ce->next;
	}
}

/*
 * dump_stats()
 */
static void dump_stats(void)
{
	printf("Opcode frequencies\n");
	printf("------------------\n\n");

	printf("Instructions: %d\n\n", total_insns);

	/*
	 * Dump out statistics about the opcodes supported by this ISA.
	 */
	struct opcode *o = opcodes;
	for (int i = 0; i <= OP_UNUSED; i++, o++) {
		if (o->isa_bitmap & (1 << (isa_version - 1))) {
			printf("%12s: %7d (%7.4f)%%\n", opcodes[i].name, opcode_counts[i],
				(float)(opcode_counts[i] / (float)total_insns) * 100.0);
		}
	}
}

/*
 * main()
 */
int main(int argc, const char *argv[])
{
	if (argc != 2) {
		fprintf(stderr, "usage: sinister <file>\n");
		exit(-1);
	}

	if (!elf_unpack(argv[1])) {
		exit(-1);
	}

	dump_elf();
	dump_stats();

	/*
	 * We really want to clean up resources but for now just let the process exit
	 * do this for us instead.
	 */

	return 0;
}

