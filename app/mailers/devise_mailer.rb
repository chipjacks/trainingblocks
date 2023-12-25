class DeviseMailer < Devise::Mailer
  layout 'mailer'
  default from: 'runoteam@gmail.com'
end
