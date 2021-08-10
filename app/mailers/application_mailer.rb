class ApplicationMailer < ActionMailer::Base
  layout 'mailer'
  default from: 'no-reply@rhinolog.app'
end
