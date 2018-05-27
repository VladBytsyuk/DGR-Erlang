gnome-terminal -e "erl -sname 'barrier@vbytsyuk-xps15' -run dgr start_barrier"

sleep 2s

gnome-terminal -e "erl -sname 'alice@vbytsyuk-xps15' -run run server 'a' 10000 'barrier@vbytsyuk-xps15'"
gnome-terminal -e "erl -sname 'client_alice@vbytsyuk-xps15' -run run client 'alice@vbytsyuk-xps15' 1 200"

sleep 5s

gnome-terminal -e "erl -sname 'bob@vbytsyuk-xps15' -run run server 'b' 10000 'barrier@vbytsyuk-xps15' 'alice@vbytsyuk-xps15'"
gnome-terminal -e "erl -sname 'client_bob@vbytsyuk-xps15' -run run client 'bob@vbytsyuk-xps15' 201 400"

sleep 10s

gnome-terminal -e "erl -sname 'carol@vbytsyuk-xps15' -run run server 'c' 10000 'barrier@vbytsyuk-xps15' 'bob@vbytsyuk-xps15'"
gnome-terminal -e "erl -sname 'client_carol@vbytsyuk-xps15' -run run client 'carol@vbytsyuk-xps15' 401 600"

sleep 15s

gnome-terminal -e "erl -sname 'derek@vbytsyuk-xps15' -run run server 'd' 10000 'barrier@vbytsyuk-xps15' 'carol@vbytsyuk-xps15'"
gnome-terminal -e "erl -sname 'client_derek@vbytsyuk-xps15' -run run client 'derek@vbytsyuk-xps15' 601 800"
