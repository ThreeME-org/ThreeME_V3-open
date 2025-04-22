import re

def parse_lines(lines):
    # Remove comments
    parsed = [re.sub(r'#.*$', '', l).strip() for l in lines]
    # Remove blank lines
    parsed = [l for l in parsed if len(l) > 0]

    # Merge continuations
    def append_continuation(lines, l):
        # Last line appended was a continuation
        if len(lines) > 0 and lines[-1][-1] == '_':
            lines[-1] = lines[-1][0:-1] + l.lstrip()
            # Otherwise, just append the new line
        else:
            lines.append(l)
        return lines

    return reduce(append_continuation, parsed, [])
