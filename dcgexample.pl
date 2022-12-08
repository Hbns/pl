startcommand --> [start,1,player].
startcommand --> [start] ,number_gt_1 ,[players].

stopcommand --> [stop].

savecommand --> [save], saveable_thing.

saveable_thing --> [game].
saveable_thing --> [player],number.

number --> [X], {integer(X), X>0}.
number_gt_1 --> [X], {integer(X), X>1}.

command --> startcommand.
command --> stopcommand.
command --> savecommand.
