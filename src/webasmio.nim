when not defined(js):
  {.error: "Webasmio is only support via the JS backend.".}

import webasmio/gen as webasmio_gen
export webasmio_gen