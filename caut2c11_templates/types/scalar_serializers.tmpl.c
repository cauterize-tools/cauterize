enum caut_status pack_{{tyInfName}}(struct caut_pack_iter * iter, {{tyInfDecl}} const * const obj) {
  if (caut_pack_iter_remaining(iter) < sizeof(*obj)) {
    return caut_status_iter_overflow;
  }

  intrinsic_{{tyInfName}}_pack(&iter->buffer[iter->pos], obj);
  return caut_status_ok;
}

enum caut_status unpack_{{tyInfName}}(struct caut_unpack_iter * iter, {{tyInfDecl}} * obj) {
  if (caut_unpack_iter_remaining(iter) < sizeof(*obj)) {
    return caut_status_iter_underflow;
  }

  intrinsic_{{tyInfName}}_unpack(&iter->buffer[iter->pos], obj);
  return caut_status_ok;
}
