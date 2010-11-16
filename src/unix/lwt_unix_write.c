/* Lightweight thread library for Objective Caml
 * http://www.ocsigen.org/lwt
 * Module Lwt_unix_write
 * Copyright (C) 2010 Jérémie Dimino
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as
 * published by the Free Software Foundation, with linking exceptions;
 * either version 2.1 of the License, or (at your option) any later
 * version. See COPYING file for details.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
 * 02111-1307, USA.
 */

#include "lwt_unix.h"

#if defined(LWT_ON_WINDOWS)

CAMLprim value lwt_unix_write()
{
  invalid_argument("not implemented");
}

CAMLprim value lwt_unix_write_launch()
{
  invalid_argument("not implemented");
}

CAMLprim value lwt_unix_write_finish()
{
  invalid_argument("not implemented");
}

#else /* defined(LWT_ON_WINDOWS) */

CAMLprim value lwt_unix_write(value val_fd, value val_buf, value val_ofs, value val_len)
{
  int ret;
  ret = write(Int_val(val_fd), &Byte(String_val(val_buf), Long_val(val_ofs)), Long_val(val_len));
  if (ret == -1) uerror("write", Nothing);
  return Val_int(ret);
}

struct job {
  LWT_UNIX_JOB_FIELDS;
  int fd;
  char *buffer;
  int length;
  int result;
  int error_code;
};

#define Job_val(v) *(struct job**)Data_custom_val(v)

static void worker(struct job *job)
{
  job->result = write(job->fd, job->buffer, job->length);
  job->error_code = errno;
}

CAMLprim value lwt_unix_write_job(value val_fd, value val_string, value val_offset, value val_length)
{
  struct job *job = lwt_unix_new(struct job);
  long length = Long_val(val_length);
  job->worker = (lwt_unix_job_worker)worker;
  job->fd = Int_val(val_fd);
  job->buffer = (char*)lwt_unix_malloc(length);
  memcpy(job->buffer, String_val(val_string) + Long_val(val_offset), Long_val(val_length));
  job->length = length;
  return lwt_unix_alloc_job((lwt_unix_job)job);
}

CAMLprim value lwt_unix_write_result(value val_job)
{
  return Val_long((Job_val(val_job))->result);
}

CAMLprim value lwt_unix_write_free(value val_job)
{
  struct job *job = Job_val(val_job);
  free(job->buffer);
  free(job);
  return Val_unit;
}

#endif /* defined(LWT_ON_WINDOWS) */
