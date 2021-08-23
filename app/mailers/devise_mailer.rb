class DeviseMailer < Devise::Mailer
  layout 'mailer'
  default from: 'no-reply@rhinolog.app'
end
