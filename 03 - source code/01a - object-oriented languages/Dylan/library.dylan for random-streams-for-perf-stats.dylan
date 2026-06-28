Module: dylan-user
Synopsis: Module and library definition for simple executable application

define library random-streams-for-perf-stats
  use common-dylan;
  use io, import: { format-out };
  use io;                 // 2026-06-27
  use system;             // 2026-06-27
end library;

define module random-streams-for-perf-stats
  use common-dylan;
  use simple-random;      // 2026-06-27
  use common-extensions;  // 2026-06-27
  use format-out;
  use streams;            // 2026-06-27
  use file-system;        // 2026-06-27
end module;
