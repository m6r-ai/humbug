/*
 * menai_vm_format.h — number-to-string formatting for the Menai VM.
 *
 * Provides shortest round-trip formatting for float and complex values,
 * matching Python's str() output.  No Python API is used.
 */
#ifndef MENAI_VM_FORMAT_H
#define MENAI_VM_FORMAT_H

MenaiValue *menai_format_float(double v);
MenaiValue *menai_format_complex(double real, double imag);

#endif /* MENAI_VM_FORMAT_H */
