gnome-terminal -e "erl -sname 'alice@vbytsyuk-xps15' -run run server 0 'empty'"
gnome-terminal -e "erl -sname 'bob@vbytsyuk-xps15' -run run server 1 'alice@vbytsyuk-xps15'"
gnome-terminal -e "erl -sname 'carol@vbytsyuk-xps15' -run run server 2 'alice@vbytsyuk-xps15'"
gnome-terminal -e "erl -sname 'derek@vbytsyuk-xps15' -run run server 3 'carol@vbytsyuk-xps15'"
gnome-terminal -e "erl -sname 'erik@vbytsyuk-xps15' -run run server 4 'bob@vbytsyuk-xps15' 'derek@vbytsyuk-xps15'"
gnome-terminal -e "erl -sname 'fred@vbytsyuk-xps15' -run run server 5 'carol@vbytsyuk-xps15' 'erik@vbytsyuk-xps15'"
gnome-terminal -e "erl -sname 'george@vbytsyuk-xps15' -run run server 6 'derek@vbytsyuk-xps15'"

