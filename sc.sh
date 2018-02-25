gnome-terminal -e "erl -sname 'server@vbytsyuk-xps15' -run run server 0 'empty'"
gnome-terminal -e "erl -sname 'client@vbytsyuk-xps15' -run run client 'server@vbytsyuk-xps15'"

