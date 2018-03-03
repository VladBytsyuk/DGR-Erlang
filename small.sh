gnome-terminal -e "erl -sname 'alice@vbytsyuk-xps15' -run run server 0 'empty'"
gnome-terminal -e "erl -sname 'client_alice@vbytsyuk-xps15' -run run client 'alice@vbytsyuk-xps15'"
gnome-terminal -e "erl -sname 'bob@vbytsyuk-xps15' -run run server 0 'alice@vbytsyuk-xps15'"
gnome-terminal -e "erl -sname 'client_bob@vbytsyuk-xps15' -run run client 'bob@vbytsyuk-xps15'"
gnome-terminal -e "erl -sname 'carol@vbytsyuk-xps15' -run run server 0 'alice@vbytsyuk-xps15'"
gnome-terminal -e "erl -sname 'client_carol@vbytsyuk-xps15' -run run client 'carol@vbytsyuk-xps15'"
gnome-terminal -e "erl -sname 'derek@vbytsyuk-xps15' -run run server 0 'alice@vbytsyuk-xps15' 'carol@vbytsyuk-xps15'"
gnome-terminal -e "erl -sname 'client_derek@vbytsyuk-xps15' -run run client 'derek@vbytsyuk-xps15'"

