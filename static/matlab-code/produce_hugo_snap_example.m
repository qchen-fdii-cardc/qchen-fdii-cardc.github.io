cellfun(@(f)fprintf("```matlab\n{{%s codesnap %s/static/matlab-code/+%s.m%s %s}}\n```\n", '%', '"', replace(f, '.', '/'), '"', '%'), filenames)
