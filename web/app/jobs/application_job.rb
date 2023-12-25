class ApplicationJob < ActiveJob::Base
  include Rollbar::ActiveJob

  retry_on StravaClient::ApiError, wait: :exponentially_longer do |job, error|
    logger.error "Strava API exception: #{error.response_body}"
    Rollbar.error(
      error,
      error.response_body,
      job: job.class.name,
      job_id: job.job_id,
      use_exception_level_filters: true,
      arguments: job.arguments.map { |a| a.as_json },
    )
    raise e
  end
end
