// Copyright 2014 the V8 project authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.


#include "src/unicode-inl.h"
#include "src/unicode-decoder.h"
#include <stdio.h>
#include <stdlib.h>

namespace unibrow {

uint16_t Utf8Iterator::operator*() {
  if (char_ > Utf16::kMaxNonSurrogateCharCode) {
    return trailing_ ? Utf16::TrailSurrogate(char_)
                     : Utf16::LeadSurrogate(char_);
  }
  DCHECK_EQ(trailing_, false);
  return static_cast<uint16_t>(char_);
}

uint16_t Utf8Iterator::operator++() {
  if (char_ > Utf16::kMaxNonSurrogateCharCode && !trailing_) {
    trailing_ = true;
    return **this;
  }
  trailing_ = false;
  offset_ = cursor_;

  Utf8::State state = Utf8::State::kAccept;
  uint32_t buffer = 0;
  while (cursor_ < static_cast<size_t>(stream_.length())) {
    char_ =
        Utf8::ValueOfIncremental(stream_[cursor_], &cursor_, &state, &buffer);
    if (char_ == Utf8::kIncomplete) continue;
    return **this;
  }

  char_ = Utf8::ValueOfIncrementalFinish(&state);
  return **this;
}

uint16_t Utf8Iterator::operator++(int) {
  uint16_t c = **this;
  ++*this;
  return c;
}

bool Utf8Iterator::Done() {
  return offset_ >= static_cast<size_t>(stream_.length());
}

void Utf8DecoderBase::Reset(uint16_t* buffer, size_t buffer_length,
                            const v8::internal::Vector<const char>& stream) {
  size_t utf16_length = 0;

  Utf8Iterator it = Utf8Iterator(stream);
  // Loop until stream is read, writing to buffer as long as buffer has space.
  while (utf16_length < buffer_length && !it.Done()) {
    utf16_length++;
    *buffer++ = it++;
  }
  bytes_read_ = it.Offset();
  trailing_ = it.Trailing();
  bytes_written_ = utf16_length;

  // Now that writing to buffer is done, we just need to calculate utf16_length
  while (!it.Done()) {
    utf16_length++;
    ++it;
  }
  utf16_length_ = utf16_length;
}

void Utf8DecoderBase::WriteUtf16Slow(uint16_t* data, size_t data_length,
                                     Utf8Iterator* it) {
  while (!it->Done()) {
    DCHECK_GT(data_length, 0);
    data_length--;
    *data++ = (*it)++;
  }
}

}  // namespace unibrow
