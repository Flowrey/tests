import React, { useState } from "react";
import Navbar from 'react-bootstrap/Navbar';
import Form from 'react-bootstrap/Form';
import Image from 'react-bootstrap/Image'
import Button from 'react-bootstrap/Button';
import Container from 'react-bootstrap/Container';
const api_root_url = "https://musicbrainz.org/ws/2";

async function search(type, query, limit = 25) {
  let obj;
  console.log(query);
  let url = `${api_root_url}/${type}/?query=${query}&fmt=json&limit=${limit}`
  console.log(url);
  const res = await fetch(url);
  obj = await res.json();

  return obj
}

async function fetch_coverart_url_from_mbid(mbid) {
  let obj;
  let url = `https://coverartarchive.org/release-group/${mbid}/front-250`;

  const res = await fetch(url);
  if (res.ok) {
    obj = await res.url;
  }

  return obj
}

function App() {
  const [query, setQuery] = useState("")
  const [coverarts, setCoverArts] = useState([])

  function handleChange(e) {
    setQuery(e.target.value)
  }

  async function changeCoverarts(release_group) {
    let coverart_url = await fetch_coverart_url_from_mbid(release_group.id);
    if (coverart_url) {
      const newCoverart = { id: release_group.id, url: coverart_url, title: release_group.title };
      coverarts.push(newCoverart);
      setCoverArts([...coverarts]);
    }
  }

  async function handleSubmit(e) {
    // Avoid internal GET request to launch
    e.preventDefault();

    // Empty previous submit
    coverarts.length = 0;
    setCoverArts([]);

    // opt 1
    // Search directly release group

    // let res = await search("release-group", query);
    // let promiseArray = []
    // for (const release_group of res["release-groups"]) {
    //   promiseArray.push(changeCoverarts(release_group));
    // }
    
    // opt 2
    // Firstly search for artist and the find artist's release
    let promiseArray = []
    let artists = await search("artist", query, 1);
    for (const artisit of artists["artists"]) {
      let release_groups = await search("release-group", `arid:${artisit.id}`)
      for (const release_group of release_groups["release-groups"]) {
        promiseArray.push(changeCoverarts(release_group));
      }
    }

    // Execute all promise and reset query
    await Promise.all(promiseArray);
    setQuery("");
  }

  const coverartsList = coverarts.map((coverart) => (
    <div className="cover" fluid="true">
      <Image
        fluid="true"
        thumbnail="true"
        src={coverart.url}
        key={coverart.id}
        className="coverart"
        alt={coverart.title}
        title={coverart.title}
        width="250px"
        height="250px"
      />
      <p>{coverart.title}</p>
    </div>
  ));

  return (
    <div className="App">
      <Navbar bg="dark" variant="dark">
        <Container fluid>
          <Navbar.Brand href="https://github.com/Flowrey">YouTueBrainz</Navbar.Brand>
          <Navbar.Collapse className="justify-content-end">
            <Form className="d-flex" onSubmit={handleSubmit}>
              <Form.Control
                type="test"
                placeholder="Search for an Artist"
                value={query}
                onChange={handleChange}
              />
              <Button variant="success" type="submit" onSubmit={handleSubmit}>Search</Button>
            </Form>
          </Navbar.Collapse>
          <div className="query">
          </div>
        </Container>
      </Navbar>
      <div className="App-coverart">
        {coverartsList}
      </div>
    </div>
  );
}

export default App;
