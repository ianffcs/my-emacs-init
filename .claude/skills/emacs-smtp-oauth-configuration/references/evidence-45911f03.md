Check smtpmail-smtp-user is the same as USER when set.
When sending mails, some auth-source query results from some smtpmail
authentication methods don't contain the :user field (meanwhile queries
from Gnus seems to always include :user).  When using predefined
provider credentials, only the :user field is different to distinguish
among different accounts, which is unfortunately missing in certain
cases.  Fortunately, smtpmail may set smtpmail-smtp-user to the user
value when X-Message-SMTP-Method is properly set.  Therefore
additionally, assuming X-Message-SMTP-Method is set correctly, we need
to check whether smtpmail-smtp-user is the same as :user to be sure.