/*---------------------------------------------------------------------------*\
 *  Copyright (c) 2023-2025 José Antonio Verde Jiménez  All Rights Reserved  *
 *---------------------------------------------------------------------------*
 * File:    application.c                                                    *
 * Author:  José Antonio Verde Jiménez  <joseaverde@protonmail.com>          *
 * License: European Union Public License 1.2                                *
\*---------------------------------------------------------------------------*/

#include "detector.h"

#include <stdio.h>

/* *** ==== Main function ============================================== *** */

#if defined(IDF_PROJECT)
void app_main (void)
#else
int main ()
#endif
{
  puts("Hello, World!");
  for (;;) { }
}
