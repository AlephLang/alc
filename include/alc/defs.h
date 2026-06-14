#ifndef __ALC_DEFS_H__
#define __ALC_DEFS_H__

#include <stddef.h>
#include <stdint.h>
#include <stdio.h>

typedef uint8_t u8;
typedef uint16_t u16;
typedef uint32_t u32;
typedef uint64_t u64;

typedef int8_t s8;
typedef int16_t s16;
typedef int32_t s32;
typedef int64_t s64;

typedef float f32;
typedef double f64;

#ifdef __cplusplus
typedef bool b8;
#else
typedef _Bool b8;
#define true 1
#define false 0
#endif

typedef size_t usize;

typedef intptr_t sptr;
typedef uintptr_t uptr;

#ifdef __cplusplus
#define __ALC_CPP_GUARD_TOP() extern "C" {
#define __ALC_CPP_GUARD_BOTTOM() }
#else
#define __ALC_CPP_GUARD_TOP()
#define __ALC_CPP_GUARD_BOTTOM()
#endif

#if defined(__cplusplus)
#define ALC_LIKELY(_expr) ((_expr)) [[likely]]
#define ALC_UNLIKELY(_expr) ((_expr)) [[unlikely]]
#elif defined(__clang__) && __STDC_VERSION__ >= 202000
#define ALC_LIKELY(_expr) ((_expr)) [[clang::likely]]
#define ALC_UNLIKELY(_expr) ((_expr)) [[clang::unlikely]]
#elif defined(__GNUC__)
#define ALC_LIKELY(_expr) (__builtin_expect(!!(_expr), 1))
#define ALC_UNLIKELY(_expr) (__builtin_expect(!!(_expr), 0))
#else
#define ALC_LIKELY(_expr) ((_expr))
#define ALC_UNLIKELY(_expr) ((_expr))
#endif

#if defined(_MSC_VER)
#include <intrin.h>
#define __alc_trap() __debugbreak()
#define __alc_noreach() __assume(0)
#define __alc_assume(_expr) __assume((_expr))
#elif defined(__GNUC__) || defined(__clang__)
#define __alc_trap() __builtin_trap()
#define __alc_noreach() __builtin_unreachable()
#define __alc_assume(_expr) __attribute__((assume((_expr))))
#else
#include <stdlib.h>
#define __ALC_TRAP_CODE 0x14
#define __ALC_NOREACH_CODE 0x88
#define __alc_trap() exit(__ALC_TRAP_CODE)
#define __alc_noreach() exit(__ALC_NOREACH_CODE)
#define __alc_assume(_expr)
#endif

#define ALC_TODO(_msg)                                                    \
  {                                                                       \
    fprintf(stderr, "(%s:%i): Not implemented yet: " _msg "\n", __FILE__, \
            __LINE__);                                                    \
    __alc_trap();                                                         \
  }

#ifdef _DEBUG
#define ALC_NOREACH()                                                         \
  {                                                                           \
    fprintf(stderr, "(%s:%i): Reached point that should never be reached.\n", \
            __FILE__, __LINE__);                                              \
    __alc_trap();                                                             \
  }
#else
#define ALC_NOREACH() __alc_noreach()
#endif

#ifdef _DEBUG
#define ALC_ASSUME(_expr)                                                 \
  {                                                                       \
    if ALC_UNLIKELY (!(_expr)) {                                          \
      fprintf(stderr, "(%s:%i): Assumption \"" #_expr "\" is not met.\n", \
              __FILE__, __LINE__);                                        \
      __alc_trap();                                                       \
    }                                                                     \
  }
#else
#define ALC_ASSUME(_expr) __alc_assume((_expr))
#endif

#define ALC_ASSERT(_expr)                                                      \
  {                                                                            \
    if ALC_UNLIKELY (!(_expr)) {                                               \
      fprintf(stderr, "(%s:%i): Assertion \"" #_expr "\" failed.\n", __FILE__, \
              __LINE__);                                                       \
      __alc_trap();                                                            \
    }                                                                          \
  }

#ifdef _DEBUG
#define ALC_UNUSED_DEBUG(_X) (void)(_X)
#endif
#define ALC_UNUSED_PERMIT(_X) (void)(_X)

#if defined(_MSC_VER)
#define ALC_API __declspec(dllexport)
#elif defined(__GNUC__) || defined(__clang__)
#define ALC_API __attribute__((visibility("default")))
#else
#define ALC_API
#endif

#ifndef __cplusplus
#define nullptr (void *)0
#endif

#endif // __ALC_DEFS_H__
