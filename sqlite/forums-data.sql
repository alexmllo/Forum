
INSERT INTO forums (id, category, title, description, moderatorId, created, topicCount, postCount, lastPostId)
  VALUES ( 1, '', 'Fòrum de prova'
         , 'Fòrum inicial en el que plantejar qüestions i respostes de prova.'
         , 1, '2020-11-01 12:00:00'
         , 1, 2, 1
         );

INSERT INTO topics (id, forumId, subject, userId, started, postCount, firstPostId, lastPostId)
  VALUES ( 1, 1, 'Pregunta de prova'
         , 1, '2020-11-01 12:00:00'
         , 2, 1, 2
         );

INSERT INTO posts (id, topicId, userId, posted, message)
  VALUES ( 1, 1, 1, '2020-11-01 12:00:00'
         , 'Missatge de la pregunta de prova.'
         ),
         ( 2, 1, 2, '2020-11-01 12:00:00'
         , 'Missatge de resposta de prova.'
         );

