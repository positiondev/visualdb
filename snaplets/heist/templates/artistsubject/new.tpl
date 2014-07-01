<apply template="page">

  <div class="container">

    <dfForm role="form" action="${artistSubjectsCreatePath}?artist_id=${artist_id}">
      <div class="form-group">
        <dfLabel ref="subjectId">Subject</dfLabel>
        <dfInputSelect class="form-control" ref="subjectId"/>
        <dfInputHidden ref="artistId" value="${artist_id}"/>
      </div>
      <dfInputSubmit class="btn"/>
    </dfForm>

  </div>

</apply>
