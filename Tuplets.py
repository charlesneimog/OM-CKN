tuplet = [1, [4, 1, None], [4, 1, None], [4, 1, None], [4, 1, None], [4, 1, None], [4, 1, 'stop']]


def fix_nested_tuplets(tuplet):
    nested = True
    normal_notes = None
    actual_pulses = []
    final_tree = []
    for x in tuplet:
        if type(x) == list:
            normal_notes = x[0]
            actual_pulses.append(x[1])
            stop_nested = x[2]
            if stop_nested == 'stop':
                final_nested_tree = [normal_notes, actual_pulses]
                final_tree.append(final_nested_tree)
        else:
            final_tree.append(x)
    return final_tree

print(fix_nested_tuplets(tuplet))