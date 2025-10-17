import benchexec
import benchexec.result as result


class Tool(benchexec.tools.template.BaseTool2):
    def executable(self, tool_locator):
        return tool_locator.find_executable("sh")

    def version(self, executable):
        return "0.1"

    def name(self):
        return "KTSN"

    def project_url(self):
        return "https://github.com/pepega007xd/ktsn"

    def get_direct_args(self, input_file):
        return f"frama-c -ktsn -ktsn-benchmark-mode -ktsn-no-catch-exceptions {input_file}"

    def get_args(self, input_file, ulevel):
        return f"frama-c -scf -ulevel={ulevel} {input_file} -then-replace -ktsn -ktsn-benchmark-mode -ktsn-no-catch-exceptions"

    def cmdline(self, executable, options, task, rlimits):
        input_file = task.input_files[0]
        input_file = input_file[:-1] + "c"  # use .c file instead of .i file

        if "direct" in options:
            return self.get_direct_args(input_file).split()

        args_ulevel_3 = self.get_args(input_file, 3)
        args_ulevel_2 = self.get_args(input_file, 2)
        args_ulevel_0 = self.get_args(input_file, 0)

        return [executable, "-c", f"timeout {rlimits.cputime // 3} {args_ulevel_3} || timeout {rlimits.cputime // 3} {args_ulevel_2} || {args_ulevel_0}"]

    def determine_result(self, run):
        if run.exit_code.value != 0:
            return result.RESULT_ERROR

        if run.output.any_line_contains("Invalid_deref"):
            return result.RESULT_FALSE_DEREF

        if run.output.any_line_contains("Invalid_free"):
            return result.RESULT_FALSE_FREE

        if run.output.any_line_contains("Invalid_memtrack"):
            return result.RESULT_FALSE_MEMTRACK

        if run.output.any_line_contains("unknown result"):
            return result.RESULT_UNKNOWN

        return result.RESULT_TRUE_PROP

    def get_value_from_output(self, output, identifier):
        output = str(output)
        if identifier == "astraltime":
            return next((s.split()[3] for s in output.splitlines()
                         if "Astral time" in s), "")

        return ""
