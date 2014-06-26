<apply template="base">

  <ifLoggedIn>
    <allArtists>
      <a href="${artistsPath}/${id}"><name/></a><brA/>
    </allArtists>
    <p><a href="/logout">Logout</a></p>
  </ifLoggedIn>

  <ifLoggedOut>
    <apply template="_login"/>
  </ifLoggedOut>

</apply>
