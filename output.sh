gnome-terminal -e "erl -sname 'barrier@vbytsyuk-xps15' -run dgr start_barrier"

gnome-terminal -e "erl -sname 'alice@vbytsyuk-xps15' -run run server 'a' 3 'barrier@vbytsyuk-xps15'"
gnome-terminal -e "erl -sname 'client_alice@vbytsyuk-xps15' -run run client 'alice@vbytsyuk-xps15'"

gnome-terminal -e "erl -sname 'bob@vbytsyuk-xps15' -run run server 'b' 3 'barrier@vbytsyuk-xps15'"
gnome-terminal -e "erl -sname 'client_bob@vbytsyuk-xps15' -run run client 'bob@vbytsyuk-xps15'"

gnome-terminal -e "erl -sname 'carol@vbytsyuk-xps15' -run run server 'c' 3 'barrier@vbytsyuk-xps15' 'alice@vbytsyuk-xps15'"
gnome-terminal -e "erl -sname 'client_carol@vbytsyuk-xps15' -run run client 'carol@vbytsyuk-xps15'"

gnome-terminal -e "erl -sname 'derek@vbytsyuk-xps15' -run run server 'd' 3 'barrier@vbytsyuk-xps15'"
gnome-terminal -e "erl -sname 'client_derek@vbytsyuk-xps15' -run run client 'derek@vbytsyuk-xps15'"

gnome-terminal -e "erl -sname 'erik@vbytsyuk-xps15' -run run server 'e' 3 'barrier@vbytsyuk-xps15' 'bob@vbytsyuk-xps15' 'derek@vbytsyuk-xps15'"
gnome-terminal -e "erl -sname 'client_erik@vbytsyuk-xps15' -run run client 'erik@vbytsyuk-xps15'"

gnome-terminal -e "erl -sname 'fred@vbytsyuk-xps15' -run run server 'f' 3 'barrier@vbytsyuk-xps15' 'carol@vbytsyuk-xps15'"
gnome-terminal -e "erl -sname 'client_fred@vbytsyuk-xps15' -run run client 'fred@vbytsyuk-xps15'"

gnome-terminal -e "erl -sname 'george@vbytsyuk-xps15' -run run server 'g' 3 'barrier@vbytsyuk-xps15' 'derek@vbytsyuk-xps15'"
gnome-terminal -e "erl -sname 'client_george@vbytsyuk-xps15' -run run client 'george@vbytsyuk-xps15'"

